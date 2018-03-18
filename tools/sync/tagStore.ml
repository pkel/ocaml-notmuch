module type M = sig
  val update : string -> string list -> unit
end

module type Cfg = sig
  val location : string
end

module Make(Cfg:Cfg) : M = struct
  open Lwt.Infix
	open Irmin_unix

	module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

	let config = Irmin_git.config ~bare:true Cfg.location
	let info fmt = Irmin_unix.info ~author:"me <me@moi.com>" fmt

  let update id tags =
		begin
			Store.Repo.v config >>= Store.master >>= fun t ->
			Store.set t ~info:(info "Updating foo/bar") ["foo"; "bar"] "hi!"
		end
    |> Lwt_main.run
end
