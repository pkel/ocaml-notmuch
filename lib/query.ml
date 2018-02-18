open Ctypes

type db = Database.t

type exclude =
  | Flag
  | True
  | False
  | All

let exclude_to_int = function
  | Flag  -> 0
  | True  -> 1
  | False -> 2
  | All   -> 3

type sort =
  | Oldest_first
  | Newest_first
  | Message_id
  | Unsorted

let sort_to_int = function
  | Oldest_first -> 0
  | Newest_first -> 1
  | Message_id   -> 2
  | Unsorted     -> 3

type t =
  { exclude      : exclude
  ; sort         : sort
  ; query        : string
  ; exclude_tags : string list
  }

(* val from_string : string -> t *)
let from_string str =
  { exclude = True
  ; sort = Unsorted
  ; query = str
  ; exclude_tags = []
  }

(* val exclude : exclude -> t -> t *)
let set_exclude x t = { t with exclude = x }

(* val exclude_tag : string -> t -> t *)
let exclude_tag s t = { t with exclude_tags = s :: t.exclude_tags }

(* val sort : sort -> t -> t *)
let set_sort x t = { t with sort = x }

let add_tag q tag =
  match C.query_add_tag_exclude q tag with
  | 0 -> ()
  (* TODO: get error from db and present *)
  | _ -> raise (Failure "Couldn't add tags")

(* finalized queries should be executed and then destroyed *)
let finalize db query =
  let q = C.query_create db query.query in
  C.query_set_omit_excluded q (exclude_to_int query.exclude) ;
  C.query_set_sort q (sort_to_int query.sort) ;
  List.iter (add_tag q) query.exclude_tags ;
  q

module type Result = sig
  type i
  type el

  val i_t : i typ
  val null : i

  val count  : C.query -> Unsigned.UInt.t ptr -> C.status
  val search : C.query -> i ptr -> C.status

  module I : Iterate.M with type el := el and type ptr := i
end

module type Exec = sig
  (** Elements of type tmp are temporary and
   *  should NOT be returned by functions f
   *)
  type tmp

  val count  : db -> t -> int
  val fold   : db -> t -> init:'acc -> f:('acc -> tmp -> 'acc) -> 'acc
  val map    : db -> t -> f:(tmp -> 'a) -> 'a list
  val iter   : db -> t -> f:(tmp -> unit) -> unit
end

module F (A : Result) : (Exec with type tmp := A.el) = struct
  let count db query =
    let cnt_ptr = allocate uint Unsigned.UInt.zero in
    let q = finalize db query in
    let () = A.count q cnt_ptr |> Status.throw in
    let () = C.query_destroy q in
    Unsigned.UInt.to_int !@cnt_ptr

  let search db query =
    let res_ptr = allocate A.i_t A.null in
    let q = finalize db query in
    let () = A.search q res_ptr |> Status.throw in
    let () = C.query_destroy q in
    !@res_ptr

  let fold db query ~init ~f =
    search db query |> A.I.fold ~init ~f

  let map db query ~f =
    search db query |> A.I.map ~f

  let iter db query ~f =
    search db query |> A.I.iter ~f
end

(* Result: Messages *)
module Mes : (Result with type el = C.message) = struct
  type i  = C.messages
  type el = C.message

  let i_t = C.messages_t
  let null = null

  let count = C.query_count_messages
  let search = C.query_search_messages

  module I = Iterate.Messages
end
module Messages = F(Mes)

(* Result: Threads *)
module Thr : (Result with type el = C.thread) = struct
  type i  = C.threads
  type el = C.thread

  let i_t = C.threads_t
  let null = null

  let count = C.query_count_threads
  let search = C.query_search_threads

  module I = Iterate.Threads
end
module Threads  = F(Thr)


