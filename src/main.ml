open Cmdliner
open Term

let default_cmd =
  let doc = "Ocaml notmuch tools" in
  Term.(ret (`Help (`Pager, None))),
  Term.info "onm" ~version:"v1.0.2" ~doc

let () = exit @@
  eval_choice default_cmd [Move.main_cmd; Sync.main_cmd]
