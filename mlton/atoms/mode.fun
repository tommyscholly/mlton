functor Mode (S: MODE_STRUCTS): MODE =
   struct
      open S

      datatype t = Stack | Heap

      fun layout m =
         Layout.str (case m of
                        Stack => ":- stack"
                      | Heap => ":- heap")

      fun equals (m1, m2) =
         case (m1, m2) of
            (Stack, Stack) => true
          | (Heap, Heap) => true
          | _ => false

      fun join (m1, m2) =
         case (m1, m2) of
            (Heap, _) => Heap
          | (_, Heap) => Heap
          | (Stack, Stack) => Stack
   end
