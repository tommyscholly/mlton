fun stackFunc (x: int) : int :- stack_mode = x + 1

fun heapFunc (x: int) : int :- heap_mode = x * 2

fun typedStackFunc (x: int) : int :- stack_mode = x + 10

fun typedHeapFunc (x: int) : int :- heap_mode = x * 10

fun parenModeFunc (x: int) : int :- stack_mode = x + 5

fun parenTypeModeFunc (x: int) : (int) :- (heap_mode) = x * 5

fun multiClause 0 :- stack_mode = 1
  | multiClause n :- stack_mode = n * multiClause (n - 1)

fun patternFunc [] :- stack_mode = 0
  | patternFunc (x::xs) :- stack_mode = x + patternFunc xs

fun complexFunc (x: int, y: int) : int :- heap_mode =
  let
    val sum = x + y
  in
    sum * 2
  end

fun normalFunc x = x + 1

fun normalTypedFunc (x: int) : int = x + 1

fun factorialStack n :- stack_mode =
  if n <= 1 then 1
  else n * factorialStack (n - 1)

fun factorialHeap n :- heap_mode =
  if n <= 1 then 1
  else n * factorialHeap (n - 1)

fun listLengthStack (xs: int list) : int :- stack_mode =
  case xs of
    [] => 0
  | _::rest => 1 + listLengthStack rest

fun listLengthHeap (xs: int list) : int :- heap_mode =
  case xs of
    [] => 0
  | _::rest => 1 + listLengthHeap rest

val a = 5
val b = 10
val testList = [1, 2, 3, 4, 5]

val result1 = stackFunc a
val result2 = heapFunc b
val result3 = typedStackFunc a
val result4 = typedHeapFunc b
val result5 = parenModeFunc a
val result6 = parenTypeModeFunc b
val result7 = multiClause 5
val result8 = patternFunc [1, 2, 3]
val result9 = complexFunc (a, b)
val result10 = normalFunc a
val result11 = normalTypedFunc a
val result12 = factorialStack 5
val result13 = factorialHeap 5
val result14 = listLengthStack testList
val result15 = listLengthHeap testList
