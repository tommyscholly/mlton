fun fib n =
    let
        val m = 1000003

        fun loop (0, a, b) = exclave_ (a, b)
          | loop (i, a, b) =
            let
                val next = (a + b) mod m
            in
                exclave_ (loop (i - 1, b, next))
            end

        val (fib, fib') = loop (n, 0, 1)
    in
        fib
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
val depth = parseArgs args

fun repeat 0 = ()
  | repeat k = 
    let 
        val res = fib k
        val _ = print (Int.toString res ^ "\n")
    in 
        repeat (k - 1) 
    end

val _ = repeat depth
