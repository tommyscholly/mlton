functor Mode (S: MODE_STRUCTS): MODE =
   struct
      open S

      datatype t = Stack | Heap | Undetermined (* expand this to support more modes *)

      fun layout m =
         Layout.str (case m of
                        Stack => " :- stack"
                      | Heap => " :- heap"
                      | Undetermined => " :- undetermined")

      fun equals (m1, m2) =
         case (m1, m2) of
            (Stack, Stack) => true
          | (Heap, Heap) => true
          | (Undetermined, Undetermined) => true
          | _ => false

      fun join (m1, m2) =
         case (m1, m2) of
            (Heap, _) => Heap
          | (_, Heap) => Heap
          | (Stack, Stack) => Stack
          | (Undetermined, m) => m
          | (m, Undetermined) => m

      fun subsumes (parent, child) =
         case (parent, child) of
            (m, Undetermined) => SOME m
          (* if the parent is undetermined, it takes the mode of the child *)
          | (Undetermined, m) => SOME m
          (* an expression in stack mode can contain heap mode data *)
          | (Stack, Heap) => SOME Stack
          | (Heap, Stack) => NONE
          | (Heap, Heap) => SOME Heap
          | (Stack, Stack) => SOME Stack

   end
