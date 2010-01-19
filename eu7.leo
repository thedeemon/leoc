isprime(k) =  {
 frec(i, x) = if i >= x then 1 else if x % i = 0 then 0 else frec(i+1, x)
 frec(3, k)
}

loop(n, z) =
 if n > 0  
  if isprime(z) > 0  
   { print(z); loop(n-1, z+1) } 
  else 
   loop(n, z+1)
   
loop(9999, 5)
