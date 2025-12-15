val modulus = 1000000007

fun safeAdd (x, y) = (x + y) mod modulus
fun safeMul (x, y) = (x * y) mod modulus

datatype tree =
  Leaf of int
| Node of { payload: int, left: tree, right: tree }

fun buildTree 0 seed = Leaf seed
  | buildTree depth seed =
      let
        val left = buildTree (depth - 1) (safeAdd (seed, depth))
        val right = buildTree (depth - 1) (safeMul (seed + depth, depth + 3))
        val payload = safeAdd (seed, depth * depth)
      in
        Node { payload = payload, left = left, right = right }
      end

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
            val tree = buildTree depth (run * 131)
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
