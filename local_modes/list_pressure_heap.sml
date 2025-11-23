fun listPressure n =
    let
        val modulus = 1000000007
        
        fun safeAdd (x, y) = (x + y) mod modulus
        fun safeMul (x, y) = (x * y) mod modulus
        
        fun generateValue x i = 
            let
                val base = x mod 1000
                val multiplier = i mod 1000
            in
                safeMul (base, multiplier)
            end
            
        fun createTempLists 0 acc = acc
          | createTempLists i acc =
            let
                val temp1 = List.tabulate (n, fn x => generateValue x i)
                val temp2 = List.map (fn x => safeAdd (x, 1)) temp1
                
                val temp3 = List.map (fn x => safeMul (x, 2)) temp2
                val temp4 = List.map (fn x => safeAdd (x, i)) temp3
                val sum = List.foldl safeAdd 0 temp4
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
