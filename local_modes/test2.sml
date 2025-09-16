type string = Primitive.Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;

val r :- stack_mode = {x = 1, y = 2}
val t :- heap_mode = (3, 4)

val mixed_record :- stack_mode = {a = r, b = t}

val _ = print_int ("Stack record x: %d\n", #x r)
val _ = print_int ("Heap tuple 1: %d\n", #1 t)

val _ = print_int ("Mixed record a.x: %d\n", #x (#a mixed_record))
val _ = print_int ("Mixed record b.1: %d\n", #1 (#b mixed_record))
