open Cmdliner
open Term

let default_cmd =
  let doc = "ocaml notmuch tools - sync tags and reorganize maildir" in
  `Help (`Pager, None) |> const |> ret,
  info "onm" ~version:"v0.1.0" ~doc

let () = exit @@
  eval_choice default_cmd [Move.main_cmd; Sync.write_cmd; Sync.pull_cmd;
  Sync.push_cmd]
