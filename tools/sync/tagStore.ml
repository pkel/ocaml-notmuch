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
  open Lwt.Infix
	open Irmin_unix

  type tag = string
  type id = string
  type remote = string

	module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
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

  let tags_of_str = String.split_on_char '\n'
  let str_of_tags = String.concat "\n"

  let set_tags t (id, tags) =
    str_of_tags tags |> set_msg_kv t id "tags"

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
    Store.find t key >|=
      function
        | None -> None
        | Some s -> Some (tags_of_str s)

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
      Sync.pull t upstream `Set
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
