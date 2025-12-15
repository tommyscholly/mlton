fun fib n =
    let
        val m = 1000003

        (*
           Tail-recursive Fibonacci that still allocates the
           (fib(n), fib(n+1)) pair with exclave_ so the caller
           controls its lifetime.
        *)
        fun loop (0, a, b) = (a, b)
          | loop (i, a, b) =
            let
                val next = (a + b) mod m
            in
                loop (i - 1, b, next)
            end

        val (fib, fib') = loop (n, 0, 1)
        val result = fib + fib' - fib'
    in
        result
    end

fun parseArgs args =
  case args of
    [] => raise Fail "Error: no arguments provided"
  | arg :: _ =>
      (case Int.fromString (arg) of
         SOME n =>
           if n > 0 then
             n
           else raise Fail "Error: iterations must be a positive integer"
       | NONE => raise Fail "Error: iterations must be an integer")

val args = CommandLine.arguments ()
(* Use a lower number for this one (e.g. 1000) inside a repeat loop 
   because O(N) stack depth might segfault standard C stacks if N is too huge. *)
val depth = parseArgs args

(* We run the deep recursion multiple times to accumulate GC pressure *)
fun repeat 0 = ()
  | repeat k = 
    let 
        val res = fib k
        val _ = print (Int.toString res ^ "\n")
    in 
        repeat (k - 1) 
    end

val _ = repeat depth
