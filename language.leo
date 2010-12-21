# Leo language tutorial
# compile and run this program to see how it works
# comments start from symbol '#'
# program is a sequence of statements, separated by new line or ';' symbol

print(11)
print(11); print(11)

# some statements are definitions:
a = 1
b = 10
arr = new int[10] # create an array of 10 ints
alen = |arr| # alen now has the length of the array arr
print(a + alen) #  11

rng = 1..10 
# this is a range - sequence of numbers defined by its beginning and end

rng_len = |rng| # now rng_len has number of numners in rng, in this case 10

# some are assignments
a <- 3

# others statements are loops, conditionals and expressions

# ranges and arrays can be used in for loops and array copy operators
# for loop is defined by 'for' followed by a sequence of "a in b" pairs
# separated by ',' where "a" is a variable name and "b" is a range or array  
# arr.range means the range of array's indices, in this case 0..9

for x in rng, i in arr.range, y in 101..200  
  arr[i] <- x + y  # body of the loop is a sequence of statements  
end # loops ends by 'end' keyword

# another loop is 'while':
while a < b
  a <- a + 1
  b <- b - 1
end

print(a, b)

# conditionals:
if a < b then print(a) else print(b)

# different comparisons and logical operators:
# && - logical AND
# || - logical OR
if a < b && b > a || a <= b && b >= a || a = b || a != b then print(11) 

# else is not necessary when 'if' is not an expression

# definitions can have parameters, so they become functions
f1(x, y) = x + y

# here 'if' is an expression
min(a, b) = if a < b then a else b

# complex statements are made with do ... end or { ... }
f2(x) = do
  a = x + 2
  f3(y) = {     # functions can be defined locally
    b = x * y   # and use values from outer scopes
    f1(b, b)    
    # value of a complex statement is the value of its last expression
  }
  print(f3(a))
end

f2(min(2,3)) # should print 16

# functions may have other functions as arguments
iter(f, xs) = for x in xs f(x) end # apply f to all elements of xs
p(x) = print(x)
show(xs) = iter(p, xs)

# functions can be defined inplace as lambdas:
show2(xs) = iter(\x -> print(x), xs)

call2(fun1, fun2, x) = { fun1(x); fun2(x) } # call two functions with x

call2(show, show2, arr) 
# shows arr contents using two functions defined above

# arrays can be of ints and bytes
barr = new byte[|arr|] # create array of bytes the same length as arr

# .head and .tail give first and rest elements of an array or a range:
ahead = arr.head # same as arr[0]
atail = arr.tail # same as arr[1 .. |arr| - 1]

sum_all(xs, sum) = if |xs|=0 then sum else sum_all(xs.tail, sum + xs.head)
print(sum_all(arr, 0))
print(sum_all(1..10, 0)) # should print 55

# a[x..y] means part of a with indices from x to y
# in one assignment we can copy or fill arrays and subarrays:

barr[0..3] <- arr[3..6] # copy 4 values from arr to barr
barr[4..6] <- 14..16    # fill by values of a sequence
barr[7..9] <- 99        # fill by a constant
arr <- barr             # copy full array
show(arr) 

# when print() gets an array as its argument, it outputs it as sequence 
# of characters, i.e. a string
# strings can be defined using string literals
s = "hello! "
print(s)
hello = s[0..4] # this is a difinition, not a copy
for c in hello
  c <- c - 32
end
print(s) # should print 'HELLO! '

# type definitions for communicating with the host program:

type mytype = {
 f1 : int
 f2 : int32[20]
 f3 : int[10]
 f4 : byte[f1]
}

args : mytype
