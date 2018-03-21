module type M = sig
  val set_mtags_assoc : (string * string list) list -> unit Lwt.t
  val set_mtags_stream : (string * string list) Lwt_stream.t -> unit Lwt.t
  val get_tags : string -> string list option Lwt.t
end

type author = { name : string; mail : string}
module type Cfg = sig
  val location : string
  val author : author
end

module Make(Cfg:Cfg) : M = struct
  open Lwt.Infix
	open Irmin_unix

	module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

	let%lwt config =
    Irmin_git.config ~bare:false Cfg.location |> Store.Repo.v

	let info fmt =
    let author = Printf.sprintf "%s <%s>" Cfg.author.name Cfg.author.mail in
    info ~author fmt

  let key_of_id id key =
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
      | None -> Lwt.return None
      | Some tree -> f tree >|= fun x -> Some x
    in
    let%lwt str = Store.master config in
    Store.with_tree str [] ~info f_

  let set_mtags_assoc assoc =
    let f tree = Lwt_list.fold_left_s set_tags tree assoc in
    apply ~info:(info "Update tags") f

  let set_mtags_stream s =
    let f tree = Lwt_stream.fold_s (fun el acc -> set_tags acc el) s tree in
    apply ~info:(info "Update tags") f

  let get_tags id =
    let%lwt t = Store.master config in
    let key = key_of_id id "tags" in
    Store.find t key >|= function
      | None -> None
      | Some s -> Some (tags_of_str s)
end
