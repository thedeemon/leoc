flags = new byte[1000000]
maxnum = (|flags| - 1)
for x in flags
 x <- 1
end
n = 0
for i in 2..maxnum
 if 0 < flags[i] then {
  loop(m, j, delta) = if j < |m| then {
   m[j] <- 0
   loop(m, (j + delta), delta)
  } 
  loop(flags, i, i)
  n <- (n + 1)
  if n = 10001 then    print(i) 
 } 
end
 print(n)
sumall(xs) = {
 s = 0
 for x in xs
  s <- (s + x)
 end
 s
}
 print(sumall(flags))


flags = new byte[1000000]
maxnum = (|flags| - 1)
for x in flags
 x <- 1
end
n = 0
for i in 2..maxnum
 if 0 < flags[i] then {
  loop(m, j, delta) = if j < |m| then {
   m[j] <- 0
   loop(m, (j + delta), delta)
  } 
  loop(flags, i, i)
  n <- (n + 1)
  if n = 10001 then    print(i) 
 } 
end
 print(n)
 print(  {
   s_1 = 0
   for x in flags
    s_1 <- (s_1 + x)
   end
   s_1
  })


# flags : byte array
# maxnum : int
# x : byte
# n : int
# i : int
# m : byte array
# j : int
# delta : int
# s_1 : int
# x : byte
var array_2
array_2 <- new [1000000]
var flags
flags <- array_2
var maxnum
maxnum <- 999999
{
 var x_3
 var x_end3
 x_3 <- flags
 x_end3 <- (flags + 1000000)
 while x_3 < x_end3 {
  *x_3 <-b- 1
  x_3 <- (x_3 + 1)
 }
}
var n
n <- 0
{
 var i_4
 var i_end4
 i_4 <- 2
 i_end4 <- (maxnum + 1)
 while i_4 < i_end4 {
  if 0 < byte($Mem[flags + i_4]) {
   fun loop_5(m, m_len, j, delta)
   {
    if j < m_len {
     {
      var pnt_6
      pnt_6 <- (m + j)
      *pnt_6 <-b- 0
     }
     return loop_5(m, m_len, (j + delta), delta)
    } else {

    }
   }
   loop_5(flags, 1000000, i_4, i_4)
   n <- (n + 1)
   if n = 10001 {
    print(i_4)
   } else {

   }
  } else {

  }
  i_4 <- (i_4 + 1)
 }
}
print(n)
var s_1
s_1 <- 0
{
 var x_7
 var x_end7
 x_7 <- flags
 x_end7 <- (flags + 1000000)
 while x_7 < x_end7 {
  s_1 <- (s_1 + byte(*x_7))
  x_7 <- (x_7 + 1)
 }
}
print(s_1)


LOAD_DEST_R, 0,
LOAD_SRC1_V, 1000000,
NEW,
LOAD_DEST_R, 1,
LOAD_SRC1_R, 0,
MOV,
LOAD_DEST_R, 2,
LOAD_SRC1_V, 999999,
MOV,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 1,
MOV,
LOAD_SRC2_V, 1000000,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 1,
ADD,
//27, while_9:
LOAD_SRC2_R, 4,
LOAD_SRC1_R, 3,
JMPLE, 35, //then_11
JMP, 49, //endif_11
//35, then_11:
LOAD_DEST_P, 3,
LOAD_SRC1_V, 1,
MOVB,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 3,
ADD,
JMP, 27, //while_9
//49, endif_11:
//49, endloop_10:
LOAD_DEST_R, 3,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 4,
LOAD_SRC1_V, 2,
MOV,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 2,
ADD,
//66, while_12:
LOAD_SRC2_R, 5,
LOAD_SRC1_R, 4,
JMPLE, 74, //then_18
JMP, 185, //endif_18
//74, then_18:
LOAD_SRC2_R, 4,
LOAD_DEST_R, 7,
LOAD_SRC1_R, 1,
ADD,
LOAD_DEST_R, 6,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 6,
LOAD_SRC1_P, 7,
MOVB,
LOAD_SRC2_R, 6,
LOAD_SRC1_V, 0,
JMPLE, 134, //endproc_14
JMP, 176, //endif_17
//99, loop_5:
LOAD_SRC2_R, -3,
LOAD_SRC1_R, -2,
JMPLE, 107, //then_15
JMP, 133, //endif_15
//107, then_15:
LOAD_SRC2_R, -2,
LOAD_DEST_R, 0,
LOAD_SRC1_R, -4,
ADD,
LOAD_DEST_P, 0,
LOAD_SRC1_V, 0,
MOVB,
LOAD_SRC2_R, -1,
LOAD_DEST_R, 0,
LOAD_SRC1_R, -2,
ADD,
LOAD_DEST_R, -2,
LOAD_SRC1_R, 0,
MOV,
JMP, 99, //loop_5
//133, endif_15:
RET,
//134, endproc_14:
LOAD_DEST_R, 6,
LOAD_SRC1_R, 1,
MOV,
LOAD_DEST_R, 7,
LOAD_SRC1_V, 1000000,
MOV,
LOAD_DEST_R, 8,
LOAD_SRC1_R, 4,
MOV,
LOAD_DEST_R, 9,
LOAD_SRC1_R, 4,
MOV,
LOAD_SRC1_V, 10,
CALL, 99, //loop_5
LOAD_SRC2_V, 1,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 3,
ADD,
LOAD_SRC2_V, 10001,
LOAD_SRC1_R, 3,
JMPEQ, 173, //then_16
JMP, 176, //endif_16
//173, then_16:
LOAD_SRC1_R, 4,
PRINT,
//176, endif_16:
//176, endif_17:
LOAD_SRC2_V, 1,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 4,
ADD,
JMP, 66, //while_12
//185, endif_18:
//185, endloop_13:
LOAD_SRC1_R, 3,
PRINT,
LOAD_DEST_R, 4,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 1,
MOV,
LOAD_SRC2_V, 1000000,
LOAD_DEST_R, 6,
LOAD_SRC1_R, 1,
ADD,
//205, while_19:
LOAD_SRC2_R, 6,
LOAD_SRC1_R, 5,
JMPLE, 213, //then_21
JMP, 239, //endif_21
//213, then_21:
LOAD_DEST_R, 7,
LOAD_SRC1_V, 0,
MOV,
LOAD_DEST_R, 7,
LOAD_SRC1_P, 5,
MOVB,
LOAD_SRC2_R, 7,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 4,
ADD,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 5,
LOAD_SRC1_R, 5,
ADD,
JMP, 205, //while_19
//239, endif_21:
//239, endloop_20:
LOAD_SRC1_R, 4,
PRINT,
