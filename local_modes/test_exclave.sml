fun loop x = case x of
    0 => [0]
  | n => loop (n-1) @ [n]

fun test_exclave x =
        exclave_ (loop x)

val xs = test_exclave 10
val ys = test_exclave 10

val _ = print (Bool.toString (xs = ys))
