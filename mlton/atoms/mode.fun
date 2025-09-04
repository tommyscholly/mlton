functor Mode (S: MODE_STRUCTS): MODE =
   struct
      open S

      datatype t = Stack | Heap | Undetermined

      fun layout m =
         Layout.str (case m of
                        Stack => " :- stack"
                      | Heap => " :- heap"
                      | Undetermined => "")

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
            (_, Undetermined) => true
          | (Stack, _) => true
          | (Heap, Heap) => true
          | (Undetermined, _) => false
          | (Heap, Stack) => false

   end
