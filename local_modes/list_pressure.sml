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

fun listPressure n =
    let
        val modulus = 1000000007
        val chunkSize = 512
        
        fun safeAdd (x, y) = (x + y) mod modulus
        fun safeMul (x, y) = (x * y) mod modulus
        
        fun generateValue x i = 
            let
                val base = x mod 1000
                val multiplier = i mod 1000
            in
                safeMul (base, multiplier)
            end
        
        fun chunkSum i chunkStart chunkLen =
            let
                val offset = chunkStart - chunkLen
                val temp1 :- stack_ =
                    List.tabulate_exclave chunkLen (fn k => generateValue (offset + k) i)
                val temp2 :- stack_ = List.map_exclave (fn x => safeAdd (x, 1)) temp1
                val temp3 :- stack_ = List.map_exclave (fn x => safeMul (x, 2)) temp2
                val temp4 :- stack_ = List.map_exclave (fn x => safeAdd (x, i)) temp3
            in
                List.foldl safeAdd 0 temp4
            end
            
        fun createTempLists 0 acc = acc
          | createTempLists i acc =
            let
                fun processChunks remainingX currentSum =
                    if remainingX <= 0 then
                        currentSum
                    else
                        let
                            val chunkLen = Int.min (chunkSize, remainingX)
                            val chunkTotal = chunkSum i remainingX chunkLen
                            val nextSum = safeAdd (chunkTotal, currentSum)
                        in
                            processChunks (remainingX - chunkLen) nextSum
                        end
                val sum = processChunks n 0
            in
                createTempLists (i - 1) (sum :: acc)
            end
    in
        createTempLists n []
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
val iterations = parseArgs args

fun printList [] = print "\n"
  | printList (x :: xs) =
      ( print (Int.toString x ^ " ")
      ; printList xs
      )

val _ = printList (listPressure iterations)
