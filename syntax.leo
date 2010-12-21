#leo syntax
print(11)  
print(11); print(11) #separator is a new line or ;

#expressions
a = new int[10]
alen = |a|
comp1 = do print(11); alen end
comp2 = { print(11); alen }
b = if alen <= 8 then 2 else 11
arrval = new int[1,alen,3,b,5]
f = \a,b,c -> a / (b+c) - b*c 
c = f(alen, b, 4) #10, 11, 4 => -44
d = c 
e = a[3]
s = "hello"
atail = a.tail
ahead = a.head

#statements
for v1 in a, v2 in 1..10, v3 in a.range
  v1 <- v2
end # now a contains 1,2,3...10

while d < 11  
  d <- d + 1
end

print(c/4, d)

rng = 101..111
range(a,b) = a..b
fun(x,y) = x + y # expression as function body
proc(x) = print(x) # statement as function body
a <- 1..10
b <- a[0] + 10
a[2..4] <- a[5..7]
if b < c || e > c || b <= c && b >= d || e = d && b != c then print(11) 

print(b)

show(xs) = for x in xs print(x) end
show(a) # 1 2 6 7 8 6 7 8 9 10
print(|rng|)
a[range(5, 9)] <- |rng|
a[range(0,1)] <- 55
a[2..4] <- rng
show(a) # 55 55 101 102 103 11 11 11 11 11

$trash
$notrash

type mytype = {
 f1 : int
 f2 : int32[20]
 f3 : int[10]
 f4 : byte[f1]
}

args : mytype

print(999) # last output: 999 
