(* work in progress *)
fun stringPressure n =
  let
    fun buildStrings 0 acc = acc
      | buildStrings i acc =
          let
            val s1 :- stack_ = "temp_string_" ^ Int.toString (i)
            val s2 :- stack_ = s1 ^ "_suffix_" ^ Int.toString (i * 2)
            val s3 :- stack_ = String.implode (String.explode (s2))
            val s4 = String.substring (s3, 0, String.size (s3) div 2)
          in
            buildStrings (i - 1) (s4 :: acc)
          end
  in
    buildStrings n []
  end

fun parseArgs args =
  case args of
    [] =>
      ( print ("Usage: program <iterations>\n")
      ; print ("Using default value of 1000\n")
      ; 1000
      )
  | arg :: _ =>
      (case Int.fromString (arg) of
         SOME n =>
           if n > 0 then
             n
           else
             ( print ("Error: iterations must be positive\n")
             ; print ("Using default value of 1000\n")
             ; 1000
             )
       | NONE =>
           ( print ("Error: '" ^ arg ^ "' is not a valid number\n")
           ; print ("Using default value of 1000\n")
           ; 1000
           ))

val args = CommandLine.arguments ()
val iterations = parseArgs args
val string = stringPressure iterations
val _ = print (String.concat (string) ^ "\n")
