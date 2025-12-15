val modulus = 10007

fun safeAdd (x, y) = (x + y) mod modulus
fun safeMul (x, y) = (x * y) mod modulus

datatype tree =
  Leaf of int
| Node of { payload: int, left: tree, right: tree }

fun buildTree 0 seed =
        exclave_ (Leaf seed)
  | buildTree depth seed =
        exclave_ Node { 
            payload = safeAdd (seed, depth), 
            left = buildTree (depth - 1) (safeAdd (seed, depth)), 
            right = buildTree (depth - 1) (safeMul (safeAdd(seed, depth), depth))
        }

fun checksum tree =
  let
    fun loop (Leaf value, acc) = safeAdd (value, acc)
      | loop (Node {payload, left, right}, acc) =
          let
            val acc = safeAdd (payload, acc)
            val acc = loop (left, acc)
          in
            loop (right, acc)
          end
  in
    loop (tree, 0)
  end

fun treePressure iterations =
  let
    val depth = Int.max (1, Int.min (iterations div 2 + 2, 18))

    fun build 0 acc = acc
      | build run acc =
          let
            val tree :- stack_ = buildTree depth (run * 131)
            val sum = checksum tree
          in
            build (run - 1) (sum :: acc)
          end
  in
    build iterations []
  end

fun parseArgs args =
  case args of
    [] => raise Fail "Error: no arguments provided"
  | arg :: _ =>
      (case Int.fromString arg of
         SOME n =>
           if n > 0 then
             n
           else
             raise Fail "Error: iterations must be a positive integer"
       | NONE => raise Fail "Error: iterations must be an integer")

val args = CommandLine.arguments ()
val iterations = parseArgs args

fun printList [] = print "\n"
  | printList (x :: xs) =
      ( print (Int.toString x ^ " ")
      ; printList xs
      )

val _ = printList (treePressure iterations)
