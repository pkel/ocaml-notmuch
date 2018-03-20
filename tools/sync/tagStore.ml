module type M = sig
  val set_tags : (string * string list) list -> unit
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

  let set_tags_ assoc = function
    | None -> Lwt.return None
    | Some tree ->
      let upd acc (id,tags) =
        str_of_tags tags |> set_msg_kv acc id "tags"
      in
      Lwt_list.fold_left_s upd tree assoc >|= fun x -> Some x

  let set_tags assoc =
    begin
      let%lwt str = Store.master config in
      Store.with_tree str [] ~info:(info "Update tags") (set_tags_ assoc)
    end
    |> Lwt_main.run
end
