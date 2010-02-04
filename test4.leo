sum_loop(sum, xs) = if |xs| > 0 then sum_loop(sum + xs.head, xs.tail) else sum
sum_loop(0, 1..10)
arr = new int[5]
sum_loop(0, arr)

type point = { x : int; y : int }

type point_list = {
 p : point
 next : point_list
}

type rec = {
 len : int
 chars : byte[len]
 ints : int[len]
 a : int[55]
}
 
args : rec
zzz: point
