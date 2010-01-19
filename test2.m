p(x) =  print(x)
iter(f, xs) =  for x in xs
  f(x)
 end
m = new byte[10]
for i in 0..(|m| - 1)
 m[i] <- (i + 1)
end
map(f, xs) = {
 a = new int[|xs|]
 for e in a, x in xs
  e <- f(x)
 end
 a
}
b = map((x) => (x + 10), m)
show(xs) = iter(p, xs)
range = 3..7
b[range] <- m[range]
show(b)
sumall(x) = {
 sum_loop(xs, sum) = if 0 < |xs| then sum_loop(xs.tail, (sum + xs.head)) else sum
 sum_loop(x, 0)
}
 print(sumall(m))


m = new byte[10]
for i in 0..(|m| - 1)
 m[i] <- (i + 1)
end
b =  {
  a_1 = new int[|m|]
  for e in a_1, x in m
   e <-     (x + 10)
  end
  a_1
 }
range = 3..7
b[range] <- m[range]
      for x in b
               print(x)
   end
 print(  {
   sum_loop_6(xs, sum) = if 0 < |xs| then sum_loop_6(xs.tail, (sum + xs.head)) else sum
   sum_loop_6(m, 0)
  })


# m : byte array
# i : int
# a_1 : int array
# e : int
# x : byte
# b : int array
# range : range
# i_13 : int
# j_13 : int
# x : int
# xs : byte array
# sum : int
var array_7
array_7 <- new [10]
var m
m <- array_7
{
 var i_8
 var i_end8
 i_8 <- 0
 i_end8 <- 10
 while i_8 < i_end8 {
  {
   var pnt_9
   pnt_9 <- (m + i_8)
   *pnt_9 <-b- (i_8 + 1)
  }
  i_8 <- (i_8 + 1)
 }
}
var array_10
array_10 <- new [40]
var a_1
a_1 <- array_10
{
 var e_11
 var e_end11
 var x_11
 var x_end11
 e_11 <- a_1
 e_end11 <- (a_1 + 40)
 x_11 <- m
 x_end11 <- (m + 10)
 while e_11 < e_end11 && x_11 < x_end11 {
  *e_11 <- (byte(*x_11) + 10)
  e_11 <- (e_11 + 4)
  x_11 <- (x_11 + 1)
 }
}
var b
b <- a_1
{
 var i_13_14
 var i_13_end14
 var j_13_14
 var j_13_end14
 i_13_14 <- 3
 i_13_end14 <- 8
 j_13_14 <- 3
 j_13_end14 <- 8
 while i_13_14 < i_13_end14 && j_13_14 < j_13_end14 {
  {
   var pnt_15
   pnt_15 <- (b + (i_13_14 * 4))
   *pnt_15 <- byte($Mem[m + j_13_14])
  }
  i_13_14 <- (i_13_14 + 1)
  j_13_14 <- (j_13_14 + 1)
 }
}
{
 var x_16
 var x_end16
 x_16 <- b
 x_end16 <- (b + 40)
 while x_16 < x_end16 {
  print(*x_16)
  x_16 <- (x_16 + 4)
 }
}
fun sum_loop_6_17(xs, xs_len, sum)
{
 if 0 < xs_len {
  var arr_len_19
  arr_len_19 <- (xs_len - 1)
  return sum_loop_6_17((xs + 1), arr_len_19, (sum + byte(*xs)))
 } else {
  return sum
 }
}
print(sum_loop_6_17(m, 10, 0))


LOAD_DEST_R, 0,
LOAD_SRC1_V, 10,
NEW,
LOAD_DEST_R, 1,
LOAD_SRC1_R, 0,
MOV,
LOAD_DEST_R, 2,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 3,
LOAD_SRC1_V, 10,
MOV,
//20, while_23:
LOAD_SRC2_R, 3,
LOAD_SRC1_R, 2,
JMPLE, 28, //then_25
JMP, 56, //endif_25
//28, then_25:
LOAD_SRC2_R, 2,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 1,
ADD,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 2,
ADD,
LOAD_DEST_P, 4,
LOAD_SRC1_R, 5,
MOVB,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 2,
LOAD_SRC1_R, 2,
ADD,
JMP, 20, //while_23
//56, endif_25:
//56, endloop_24:
LOAD_DEST_R, 2,
LOAD_SRC1_V, 40,
NEW,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 2,
MOV,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 3,
MOV,
LOAD_SRC2_V, 40,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 3,
ADD,
LOAD_DEST_R, 6,
LOAD_SRC1_R, 1,
MOV,
LOAD_SRC2_V, 10,
LOAD_DEST_R, 7,
LOAD_SRC1_R, 1,
ADD,
//90, while_26:
LOAD_SRC2_R, 5,
LOAD_SRC1_R, 4,
JMPLE, 98, //then_30
JMP, 139, //endif_29
//98, then_30:
LOAD_SRC2_R, 7,
LOAD_SRC1_R, 6,
JMPLE, 106, //then_29
//104, else_28:
JMP, 139, //endif_29
//106, then_29:
LOAD_DEST_R, 8,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 8,
LOAD_SRC1_P, 6,
MOVB,
LOAD_SRC2_V, 10,
LOAD_DEST_P, 4,
LOAD_SRC1_R, 8,
ADD,
LOAD_SRC2_V, 4,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 4,
ADD,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 6,
LOAD_SRC1_R, 6,
ADD,
JMP, 90, //while_26
//139, endif_29:
//139, endif_30:
//139, endloop_27:
LOAD_DEST_R, 4,
LOAD_SRC1_R, 3,
MOV,
LOAD_DEST_R, 5,
LOAD_SRC1_V, 3,
MOV,
LOAD_DEST_R, 6,
LOAD_SRC1_V, 8,
MOV,
LOAD_DEST_R, 7,
LOAD_SRC1_V, 3,
MOV,
LOAD_DEST_R, 8,
LOAD_SRC1_V, 8,
MOV,
//164, while_31:
LOAD_SRC2_R, 6,
LOAD_SRC1_R, 5,
JMPLE, 172, //then_35
JMP, 232, //endif_34
//172, then_35:
LOAD_SRC2_R, 8,
LOAD_SRC1_R, 7,
JMPLE, 180, //then_34
//178, else_33:
JMP, 232, //endif_34
//180, then_34:
LOAD_SRC2_V, 4,
LOAD_DEST_R, 10,
LOAD_SRC1_R, 5,
MUL,
LOAD_SRC2_R, 10,
LOAD_DEST_R, 9,
LOAD_SRC1_R, 4,
ADD,
LOAD_SRC2_R, 7,
LOAD_DEST_R, 11,
LOAD_SRC1_R, 1,
ADD,
LOAD_DEST_R, 10,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 10,
LOAD_SRC1_P, 11,
MOVB,
LOAD_DEST_P, 9,
LOAD_SRC1_R, 10,
MOV,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 5,
ADD,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 7,
LOAD_SRC1_R, 7,
ADD,
JMP, 164, //while_31
//232, endif_34:
//232, endif_35:
//232, endloop_32:
LOAD_DEST_R, 5,
LOAD_SRC1_R, 4,
MOV,
LOAD_SRC2_V, 40,
LOAD_DEST_R, 6,
LOAD_SRC1_R, 4,
ADD,
//244, while_36:
LOAD_SRC2_R, 6,
LOAD_SRC1_R, 5,
JMPLE, 252, //then_38
JMP, 324, //endproc_39
//252, then_38:
LOAD_SRC1_P, 5,
PRINT,
LOAD_SRC2_V, 4,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 5,
ADD,
JMP, 244, //while_36
//264, sum_loop_6_17:
LOAD_SRC2_R, -2,
LOAD_SRC1_V, 0,
JMPLE, 276, //then_40
LOAD_DEST_R, -3,
LOAD_SRC1_R, -1,
MOV,
RET,
//276, then_40:
LOAD_SRC2_V, 1,
LOAD_DEST_R, 0,
LOAD_SRC1_R, -2,
SUB,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 1,
LOAD_SRC1_R, -3,
ADD,
LOAD_DEST_R, 3,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 3,
LOAD_SRC1_P, -3,
MOVB,
LOAD_SRC2_R, 3,
LOAD_DEST_R, 2,
LOAD_SRC1_R, -1,
ADD,
LOAD_DEST_R, -3,
LOAD_SRC1_R, 1,
MOV,
LOAD_DEST_R, -2,
LOAD_SRC1_R, 0,
MOV,
LOAD_DEST_R, -1,
LOAD_SRC1_R, 2,
MOV,
JMP, 264, //sum_loop_6_17
//324, endproc_39:
LOAD_DEST_R, 5,
LOAD_SRC1_R, 1,
MOV,
LOAD_DEST_R, 6,
LOAD_SRC1_V, 10,
MOV,
LOAD_DEST_R, 7,
LOAD_SRC1_V, 0,
MOV,
LOAD_SRC1_V, 8,
CALL, 264, //sum_loop_6_17
LOAD_SRC1_R, 5,
PRINT,
