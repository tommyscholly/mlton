open Primitive
open Int32

type string = Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;

fun exclave_list () :- stack_mode = exclave_ [1, 2, 3]

fun return_stack_list (l: int32 list :- stack_mode) :- stack_mode = l

val l = exclave_list ()
val ll = return_stack_list l

(* fun should_fail () :- stack_mode = *)
(*   let val l = exclave_list () *)
(*   in exclave_ l *)
(*   end *)
