module type M = sig
  type tag = string
  type id = string
  type remote = string

  val push : remote -> (unit, string) result Lwt.t
  val pull : remote -> (unit, string) result Lwt.t
  val set_mtags_assoc  : (id * tag list) list -> unit Lwt.t
  val set_mtags_stream : (id * tag list) Lwt_stream.t -> unit Lwt.t
  val get_tags : id -> string list option Lwt.t
end

type author = { name : string; mail : string}
module type Cfg = sig
  val location : string
  val author : author
end

module Make(Cfg:Cfg) : M = struct
  open Sexplib.Std
  open Lwt.Infix
  open Irmin
  open Irmin_unix

  type tag = string
  type id = string
  type remote = string

  module StringSet = Set.Make(String)

  module Entry = struct
    type t = tag list
    let t = Type.(list string)
    let pp = Fmt.(list string)
    let of_string s = Ok (String.split_on_char '\n' s)

    let threeway ~old a b =
      let open StringSet in
      let old = of_list old in
      let a   = of_list a in
      let b   = of_list b in
      let adda = diff a old in
      let addb = diff b old in
      let rema = diff old a in
      let remb = diff old b in
      let add = union adda addb in
      let rem = union rema remb in
      diff old rem |> union add |> fun set ->
        fold (fun el acc -> el::acc) set []

    let mergef ~old a b =
      Lwt_io.printf "merge\n" >>= old >|= function
      | Error e -> Error e
      | Ok None -> Ok (threeway ~old:[] a b)
      | Ok (Some l) -> Ok (threeway ~old:l a b)

    let merge = Merge.(v t mergef |> option)

  end

  module Store = Irmin_unix.Git.FS.KV(Entry)
  module Sync = Irmin.Sync(Store)

  let%lwt config =
    Irmin_git.config ~bare:false Cfg.location |> Store.Repo.v

  let info fmt =
    let author = Printf.sprintf "%s <%s>" Cfg.author.name Cfg.author.mail in
    info ~author fmt

  let tags_info = info "Update tags"

  let key_of_id id key =
    let id = B64.encode id in
    (* let h = Hashtbl.hash id in *)
    (* let a = h land 1023 in *)
    (* let sa = Printf.sprintf "%04d" a in *)
    (* sa :: *) "msg_by_id" :: id :: [key]

  let set_msg_kv tree id key value =
    Store.Tree.add tree (key_of_id id "tags") value

  let set_tags t (id, tags) =
    set_msg_kv t id "tags" tags

  let apply ~info f =
    let f_ = function
      (* None happens on nonexistent git repo, a second run gives success *)
      | None -> Lwt.return None
      | Some tree -> f tree >|= fun x -> Some x
    in
    Store.master config >>= fun t ->
    Store.with_tree t [] ~info f_

  let set_mtags_assoc assoc =
    let f tree = Lwt_list.fold_left_s set_tags tree assoc in
    apply ~info:tags_info f

  let set_mtags_stream s =
    let f tree = Lwt_stream.fold_s (fun el acc -> set_tags acc el) s tree in
    apply ~info:tags_info f

  let get_tags id =
    let key = key_of_id id "tags" in
    Store.master config >>= fun t ->
    Store.find t key

  let pull remote =
    let upstream = Irmin.remote_uri remote in
    let handlerr = function
      | `Conflict s
      | `Msg s -> Error s
      | `No_head -> Error "No head"
      | `Not_available -> Error "Not available"
    in
    Store.master config
    >>= fun t ->
      Sync.pull t upstream (`Merge (info "Merge remote"))
    >|= function
     | Ok () -> Ok ()
     | Error e -> handlerr e

  let push remote =
    let cmd = ("", [|"git"; "-C"; Cfg.location ;"push"; remote; "master"|]) in
    Lwt_process.exec cmd
    >|= function
      | WEXITED 0 -> Ok ()
      | _ -> Error "external command returned non-zero"
end
