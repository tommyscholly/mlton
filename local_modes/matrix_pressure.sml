structure List =
  struct
    open List

    fun map_exclave f [] = []
      | map_exclave f (x :: xs) =
          exclave_ ((f x :- stack_) :: (map_exclave f xs :- stack_))

    fun tabulate_exclave n f =
      if n <= 0 then
        []
      else
        exclave_ ((f n :- stack_) :: tabulate_exclave (n - 1) f)
  end

val modulus = 1000000007

fun safeAdd (x, y) = (x + y) mod modulus
fun safeMul (x, y) = (x * y) mod modulus

fun rowSeed row col workload =
  safeAdd (safeMul (row + col + 1, workload + col + 3), row * 7 + col)

fun processRow row dimension workload =
  let
    val base :- stack_ = List.tabulate_exclave dimension (fn col => rowSeed row col workload)
    val scaled :- stack_ = List.map_exclave (fn value => safeMul (value, row + 1)) base
    val biased :- stack_ = List.map_exclave (fn value => safeAdd (value, dimension)) scaled
    val mixed :- stack_ =
      List.map_exclave (fn value => safeAdd (safeMul (value, workload + row), row)) biased
  in
    List.foldl safeAdd 0 mixed
  end

fun matrixPressure iterations =
  let
    val dimension = Int.max (4, Int.min (iterations + 8, 256))

    fun build 0 acc = acc
      | build row acc =
          let
            val digest = processRow row dimension iterations
          in
            build (row - 1) (digest :: acc)
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

val _ = printList (matrixPressure iterations)
