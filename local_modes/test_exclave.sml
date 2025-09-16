open Primitive
open Int32

type string = Char8.t vector

val print_int = _import "printf" : (string * int32) -> int32;

fun returnStackInt (x: int32) : int32 =
  let
    val y :- stack_mode = + (x, 10)
  in
    exclave_ y
  end

fun returnStackTuple (x: int32, y: int32) : (int32 * int32) =
  let
    val pair :- stack_mode = (x, y)
  in
    exclave_ pair
  end

datatype Result = Success of int32 | Failure

fun returnStackResult (x: int32) : Result =
  let
    val result :- stack_mode = Success ( * (x, 2))
  in
    exclave_ result
  end

val a = 5
val b = 10

val result1 = returnStackInt a
val result2 = returnStackTuple (a, b)
val result3 = returnStackResult a

val _ = print_int ("Stack int result: %d\n\000", result1)
val _ = case result2 of (x, y) => (print_int ("Stack tuple: (%d, %d)\n\000", x); print_int ("", y))
val _ = case result3 of
          Success x => print_int ("Stack result: Success %d\n\000", x)
        | Failure => print_int ("Stack result: Failure\n\000", 0)

(* Test that should fail: exclave on heap-allocated data *)
(* Uncomment to test error checking:
fun badExclave (x: int32) : int32 :- stack_mode =
  let
    val y :- heap_mode = x + 10
  in
    exclave_ y  (* This should cause a compile error *)
  end
*)

(* Test that should fail: exclave in non-tail position *)
(* TODO: This should be caught by tail position analysis
fun badTailPosition (x: int32) : int32 =
  let
    val y :- stack_mode = x + 10
    val z = exclave_ y  (* This should cause a compile error - not in tail position *)
  in
    z + 1
  end
*)
