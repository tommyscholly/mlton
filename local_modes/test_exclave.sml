(* fun loop x = case x of *)
(*     0 => [0] *)
(*   | n => loop (n-1) @ [n] *)

(* fun test_exclave x = *)
(*         exclave_ (loop x) *)

fun test_exclave x =
        exclave_ [x, 1, 2]

fun basic_stack_alloc y =
        let val x :- stack_ = [y, 2, 3]
        in 
            y 
        end

val xs :- stack_ = test_exclave 10
val _ = print (Int.toString (List.hd xs))
val ys = test_exclave 10  
val _ = print (Int.toString (List.hd ys))
val _ = basic_stack_alloc 10
