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

fun baseString iteration idx =
  let
    val prefix :- stack_ = "seed_" ^ Int.toString iteration
    val suffix :- stack_ = "_" ^ Int.toString (idx mod 4096)
    val combined :- stack_ = prefix ^ suffix
  in
    combined
  end

fun appendSuffix iteration s =
  let
    val suffix :- stack_ = "_mix_" ^ Int.toString ((iteration * 17 + String.size s) mod 97)
    val combined :- stack_ = s ^ suffix
  in
    combined
  end

fun rotateString s shift =
  let
    val len = String.size s
  in
    if len = 0 then
      s
    else
      let
        val offset = Int.abs shift mod len
        val first :- stack_ = String.substring (s, offset, len - offset)
        val second :- stack_ = String.substring (s, 0, offset)
        val rotated :- stack_ = first ^ second
      in
        rotated
      end
  end

fun scrambleString iteration s =
  let
    val len = String.size s
  in
    if len <= 1 then
      s
    else
      let
        val mid = len div 2
        val front :- stack_ = String.substring (s, 0, mid)
        val back :- stack_ = String.substring (s, mid, len - mid)
        val frontChars :- stack_ = String.explode front
        val reversed :- stack_ = String.implode (List.rev frontChars)
        val joined :- stack_ = back ^ reversed
      in
        rotateString joined (iteration + len)
      end
  end

fun digestStrings pieces =
  List.foldl (fn (piece, acc) => safeAdd (String.size piece, acc)) 0 pieces

fun stringPressure iterations =
  let
    val batchSize = Int.max (8, Int.min (iterations, 256))

    fun build 0 acc = acc
      | build iter acc =
          let
            val seeds :- stack_ =
              List.tabulate_exclave batchSize (fn idx => baseString iter (idx + iter))
            val suffixed :- stack_ = List.map_exclave (appendSuffix iter) seeds
            val rotated :- stack_ = List.map_exclave (fn s => rotateString s (iter * 3)) suffixed
            val scrambled :- stack_ = List.map_exclave (scrambleString iter) rotated
            val digest = digestStrings scrambled
          in
            build (iter - 1) (digest :: acc)
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

val _ = printList (stringPressure iterations)
