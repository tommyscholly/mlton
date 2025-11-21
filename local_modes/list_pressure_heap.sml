structure List =
  struct
    open List

    fun map f [] = []
      | map f (x :: xs) =
          ((f x) :: (map f xs))

    fun tabulate n f =
      if n < 0 then
        []
      else
        if n = 0 then
          []
        else
            ((f n) :: tabulate (n - 1) f)
  end

fun listPressure n =
    let
        fun createTempLists 0 acc = acc
          | createTempLists i acc =
            let
                (* temporary lists that will be quickly discarded *)
                val _ = print ("tabululate\n")
                val temp1 = List.tabulate n (fn x => x * i)
                val _ = List.app (fn x => print (Int.toString x ^ " ")) temp1
                (* val temp2 :- stack_ = List.map (fn x => x + 1) temp1 *)
                val sum = List.foldl op+ 0 temp1
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
