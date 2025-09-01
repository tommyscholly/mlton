open Primitive

type string = Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;
val print_string = _import "printf" : (string * string) -> int32;
val x = 0

fun local_thing (x : int32 :- stack_mode) = 1

val str :- stack_mode = "Hello, world!\n"

(* this should not work in a successfully type checked program *)
val _ = print_int ("%d\n\000", local_thing x)
val _ = print_string ("%s\n\000", str)


