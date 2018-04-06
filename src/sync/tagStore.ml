module type M = sig
  type tag = string
  type id = string
  type remote = string
  type updates = (id * tag list) list

  val push : remote -> (unit, string) result Lwt.t
  val pull : remote -> (updates, string) result Lwt.t
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
  type updates = (id * tag list) list

  module StringSet = Set.Make(String)

  module Entry = struct
    type t = tag list
    let t = Type.(list string)

    let pp fmt lst =
      (* make tag files consistent by ordereing the tags *)
      List.sort String.compare lst |>
      Fmt.(list ~sep:Format.pp_force_newline string) fmt

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
      old () >|= function
      | Error e -> Error e
      | Ok None -> Ok (threeway ~old:[] a b)
      | Ok (Some l) -> Ok (threeway ~old:l a b)

    let merge = Merge.(v t mergef |> option)

  end

  module Store = Irmin_unix.Git.FS.KV(Entry)
  module Sync = Irmin.Sync(Store)

  let%lwt repo =
    Irmin_git.config ~bare:false Cfg.location |> Store.Repo.v

  let info fmt =
    let author = Printf.sprintf "%s <%s>" Cfg.author.name Cfg.author.mail in
    info ~author fmt

  let tags_info = info "Update tags"

  let key_prefix = "b64_id"

  let key_of_id id key =
    let b64 = B64.encode id in
    let pre = String.sub b64 0 2 in
    key_prefix :: pre :: b64 :: [key]

  let get_tag_tree str = Store.get_tree str [key_prefix]

  let diff_tag_trees old fresh : 'a list Lwt.t =
    let fmap = function
      | [_; b64; "tags"] , `Updated (_, (tags, _))
      | [_; b64; "tags"] , `Added (tags, _)
        -> Some (B64.decode b64, tags) |> Lwt.return
      (* ignore removes and other keys *)
      | _ -> Lwt.return None
    in
    let%lwt old = old
    and fresh = fresh in
    Store.Tree.diff old fresh
      >>= Lwt_list.filter_map_p fmap

  let set_msg_kv tree id key value =
    Store.Tree.add tree (key_of_id id "tags") value

  let set_tags t (id, tags) =
    set_msg_kv t id "tags" tags

  (* apply a tree transformation f to master using commit msg info *)
  let apply ~info f =
    let f_ treeopt =
      let tree = match treeopt with
        (* None happens on nonexistent git repo *)
        | None -> Store.Tree.empty
        | Some tree -> tree
      in
      f tree >|= fun x -> Some x
    in
    Store.master repo >>= fun t ->
    Store.with_tree t [] ~info f_

  let set_mtags_assoc assoc =
    let f tree = Lwt_list.fold_left_s set_tags tree assoc in
    apply ~info:tags_info f

  let set_mtags_stream s =
    let f tree = Lwt_stream.fold_s (fun el acc -> set_tags acc el) s tree in
    apply ~info:tags_info f

  let get_tags id =
    let key = key_of_id id "tags" in
    Store.master repo >>= fun t ->
    Store.find t key

  let pull remote =
    let before = Store.master repo >>= get_tag_tree in
    let upstream = Irmin.remote_uri remote in
    let handlerr = function
      | `Conflict s
      | `Msg s -> Error s
      | `No_head -> Error "No head"
      | `Not_available -> Error "Not available"
    in
    Store.master repo >>= fun mstr ->
      Sync.pull mstr upstream (`Merge (info "Merge remote"))
    >>= function
     | Error e -> handlerr e |> Lwt.return
     | Ok () ->
         let after = Store.master repo >>= get_tag_tree in
         diff_tag_trees before after >|= fun x -> Ok x

  let push remote =
    let cmd = ("", [|"git"; "-C"; Cfg.location ;"push"; remote; "master"|]) in
    Lwt_process.exec cmd
    >|= function
      | WEXITED 0 -> Ok ()
      | _ -> Error "external command returned non-zero"
end
