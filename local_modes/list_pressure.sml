structure List =
  struct
    open List

    fun map_exclave f [] = []
      | map_exclave f (x :: xs) =
          exclave_ ((f x :- stack_) :: (map_exclave f xs :- stack_))

    fun tabulate_exclave n f =
      if n < 0 then
        []
      else
        if n = 0 then
          []
        else
            exclave_ ((f n :- stack_) :: tabulate_exclave (n - 1) f)
  end

fun listPressure n =
    let
        fun createTempLists 0 acc = acc
          | createTempLists i acc =
            let
                (* temporary lists that will be quickly discarded *)
                val temp1 :- stack_ = List.tabulate_exclave n (fn x => x * i)
                val temp2 :- stack_ = List.map_exclave (fn x => x + 1) temp1
                (* val _ = print ("temp2\n") *)
                val sum = List.foldl op+ 0 temp2
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
