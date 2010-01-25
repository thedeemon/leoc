nat = new byte[20]
for i in nat.range  nat[i] <- i + 1  end  # теперь в nat числа от 1 до 20

map(f, xs) = { # составной оператор заключается в {...} или do ... end
 t = new int[ |xs| ]  
 for i in xs.range, x in xs    
  t[i] <- f(x)
 end
 t
}

iter(f, xs) = for x in xs f(x) end  # применить функцию f ко всем элементам xs

show(xs) = iter(\x -> print(x), xs) # вывод всех элементов. первый параметр - лямбда

even = map(\x -> 2 * x, nat)  
# even - массив интов с четными числами от 2 до 40

show(even)   # выводит 2 4 6 ... 40
show(1..10)  # выводит 1 2 3 ... 10
# 1..10 - это не массив и не список, это интервал, заданный двумя числами.

sq(x) = x * x
squares = map(sq, 1..10) # теперь массив squares содержит 1 4 9 .. 100

sum(xs) = do
  loop(s, xs) = if |xs| > 0 then loop(xs.head + s, xs.tail) else s
  loop(0, xs)
end

print( sum(1..10) )      # выводит 55
print( sum(nat) )        # выводит 210
print( sum(squares ) )   # выводит 385

squares[3..7] <- nat[0..4] # копирует 5 элементов из nat в squares

m = new int[9]
m <- squares # копирует 9 элементов из squares в m

rng = 6..9
m[rng] <- even[rng]
show(m)  # выводит 1 4 9 1 2 3 14 16 18
