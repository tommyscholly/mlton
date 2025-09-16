open Primitive
open Int32

type string = Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;

datatype Tree = Node of int32 * Tree * Tree | Leaf

val leaf1 :- stack_mode = Leaf
val leaf2 :- stack_mode = Leaf

val node1 :- heap_mode = Node (1, Leaf, Leaf)
val node2 :- stack_mode = Node (2, node1, Leaf)

fun sum_tree (t : Tree) : int32 =
    case t of
        Leaf => 0
      | Node (v, l, r) => + (v, + (sum_tree l, sum_tree r))

val total_sum = sum_tree node2

val _ = print_int ("Tree sum: %d\n", total_sum)
