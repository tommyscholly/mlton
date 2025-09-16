open Primitive
open Int32

type string = Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;

fun add_stack (x : int32 :- stack_mode, y : int32 :- stack_mode) : int32 = + (x, y)

fun add_heap (x : int32 :- heap_mode, y : int32 :- heap_mode) : int32 = + (x, y)

val a :- stack_mode = 10
val b :- stack_mode = 20

val c :- heap_mode = 30
val d :- heap_mode = 40

val stack_sum :- stack_mode = add_stack (a, b)
val heap_sum :- heap_mode = add_heap (c, d)

val _ = print_int ("Stack sum: %d\n", stack_sum)
val _ = print_int ("Heap sum: %d\n", heap_sum)
