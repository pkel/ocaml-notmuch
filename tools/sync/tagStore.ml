module type M = sig
  val update : (string * string list) list -> unit
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

	let config = Irmin_git.config ~bare:true Cfg.location
	let info fmt =
    let author = Printf.sprintf "%s <%s>" Cfg.author.name Cfg.author.mail in
    info ~author fmt
(*  *)
  (* let set_single t (id, tags) = *)
    (* let info = info "Updating tags" in *)
    (* let value = String.concat "|" tags in *)
    (* Store.set t ~info ["messages";id;"tags"] value *)
(*  *)
  let set_lst lst t_opt =
    let upd acc (id,tags) =
      let value = String.concat "|" tags in
      Store.Tree.add acc ["tags"; id] value
    in
    let updt t = Lwt_list.fold_left_s upd t lst in
    match t_opt with
    | None -> Lwt.return None
    | Some t -> updt t >>= fun x -> Lwt.return (Some x)

  let update lst =
		begin
			Store.Repo.v config >>= Store.master >>= fun t ->
        Store.with_tree t [] ~info:(info "Update tags") (set_lst lst)
		end
    |> Lwt_main.run
end
