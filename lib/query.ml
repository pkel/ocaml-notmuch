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

module type Iterator = sig
  type i
  type el

  val i_t : i typ
  val null : i

  val count   : C.query -> Unsigned.UInt.t ptr -> C.status
  val exec    : C.query -> i ptr -> C.status
  val valid   : i -> bool
  val get     : i -> el
  val move    : i -> unit
  val destroy : i -> unit
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

module F (I : Iterator) : (Exec with type tmp := I.el) = struct
(* TODO: destroy queries *)

  let count db query =
    (* execute count *)
    let exec final =
      let cnt_ptr = allocate uint Unsigned.UInt.zero in
      match I.count final cnt_ptr with
      | 0 -> !@cnt_ptr
             (* TODO: Get error from db and fail appropriatly *)
      | _ -> raise (Failure "Query execution failed")
    in
    finalize db query |> exec |> Unsigned.UInt.to_int

  let fold db query ~init ~f =
    (* execute search *)
    let exec final =
      let res_ptr = allocate I.i_t I.null in
      match I.exec final res_ptr with
      | 0 -> !@res_ptr
             (* TODO: Get error from db and fail appropriatly *)
      | _ -> raise (Failure "Query execution failed")
    in
    let i = finalize db query |> exec in
    (* iterate and build list *)
    let rec h acc =
      if I.valid i then
        let el = I.get i in
        I.move i ;
        h (f acc el)
      else
        acc
    in
    let res = h init in
    (* destroy c iterator object and thereby all elements
     * everyting in the c world will be lost from here on
     *)
    I.destroy i ;
    res

  let map db query ~f =
    let init = [] in
    let f acc e = (f e) :: acc in
    fold db query ~init ~f |> List.rev

  let iter db query ~f =
    let init = () in
    let f acc e = f e in
    fold db query ~init ~f
end

(* Message Iterator *)
module IM : (Iterator with type el = C.message) = struct
  type i  = C.messages
  type el = C.message

  let i_t = C.messages_t
  let null = null

  let count= C.query_count_messages ;;
  let exec = C.query_search_messages ;;
  let valid = C.messages_valid ;;
  let get = C.messages_get ;;
  let move = C.messages_move_to_next ;;
  let destroy = C.messages_destroy ;;
end

(* Thread Iterator *)
module IT : (Iterator with type el = C.thread) = struct
  type i  = C.threads
  type el = C.thread

  let i_t = C.threads_t
  let null = null

  let count= C.query_count_threads ;;
  let exec = C.query_search_threads ;;
  let valid = C.threads_valid ;;
  let get = C.threads_get ;;
  let move = C.threads_move_to_next ;;
  let destroy = C.threads_destroy ;;
end

module Messages = F(IM)
module Threads  = F(IT)


