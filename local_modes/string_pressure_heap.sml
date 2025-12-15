val modulus = 1000000007

fun safeAdd (x, y) = (x + y) mod modulus

fun baseString iteration idx =
  "seed_" ^ Int.toString iteration ^ "_" ^ Int.toString (idx mod 4096)

fun appendSuffix iteration s =
  s ^ ("_mix_" ^ Int.toString ((iteration * 17 + String.size s) mod 97))

fun rotateString s shift =
  let
    val len = String.size s
  in
    if len = 0 then
      s
    else
      let
        val offset = Int.abs shift mod len
        val first = String.substring (s, offset, len - offset)
        val second = String.substring (s, 0, offset)
      in
        first ^ second
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
        val front = String.substring (s, 0, mid)
        val back = String.substring (s, mid, len - mid)
        val reversed = String.implode (List.rev (String.explode front))
      in
        rotateString (back ^ reversed) (iteration + len)
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
            val seeds = List.tabulate (batchSize, fn idx => baseString iter (idx + iter))
            val suffixed = List.map (appendSuffix iter) seeds
            val rotated = List.map (fn s => rotateString s (iter * 3)) suffixed
            val scrambled = List.map (scrambleString iter) rotated
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
