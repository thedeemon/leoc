nums = 1..100
map_sum(f, xs) = { s = 0; for x in xs s <- s + f(x) end; s }
sq(x) = x * x
print( sq( map_sum(\x->x, nums) ) - map_sum(sq, nums) )
