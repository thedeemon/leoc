flags = new byte[10000000]
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


flags = new byte[10000000]
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


# flags : byte array
# maxnum : int
# x : byte
# n : int
# i : int
# m : byte array
# j : int
# delta : int
var array_1
array_1 <- new [10000000]
var flags
flags <- array_1
var maxnum
maxnum <- 9999999
{
 var x_2
 var x_end2
 x_2 <- flags
 x_end2 <- (flags + 10000000)
 while x_2 < x_end2 {
  *x_2 <-b- 1
  x_2 <- (x_2 + 1)
 }
}
var n
n <- 0
{
 var i_3
 var i_end3
 i_3 <- 2
 i_end3 <- (maxnum + 1)
 while i_3 < i_end3 {
  if 0 < byte($Mem[flags + i_3]) {
   fun loop_4(m, m_len, j, delta)
   {
    if j < m_len {
     {
      var pnt_5
      pnt_5 <- (m + j)
      *pnt_5 <-b- 0
     }
     return loop_4(m, m_len, (j + delta), delta)
    } else {

    }
   }
   loop_4(flags, 10000000, i_3, i_3)
   n <- (n + 1)
   if n = 10001 {
    print(i_3)
   } else {

   }
  } else {

  }
  i_3 <- (i_3 + 1)
 }
}
print(n)


LOAD_DEST_R, 0,
LOAD_SRC1_V, 10000000,
NEW,
LOAD_DEST_R, 1,
LOAD_SRC1_R, 0,
MOV,
LOAD_DEST_R, 2,
LOAD_SRC1_V, 9999999,
MOV,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 1,
MOV,
LOAD_SRC2_V, 10000000,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 1,
ADD,
//27, while_7:
LOAD_SRC2_R, 4,
LOAD_SRC1_R, 3,
JMPLE, 35, //then_9
JMP, 49, //endif_9
//35, then_9:
LOAD_DEST_P, 3,
LOAD_SRC1_V, 1,
MOVB,
LOAD_SRC2_V, 1,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 3,
ADD,
JMP, 27, //while_7
//49, endif_9:
//49, endloop_8:
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
//66, while_10:
LOAD_SRC2_R, 5,
LOAD_SRC1_R, 4,
JMPLE, 74, //then_16
JMP, 185, //endif_16
//74, then_16:
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
JMPLE, 134, //endproc_12
JMP, 176, //endif_15
//99, loop_4:
LOAD_SRC2_R, -3,
LOAD_SRC1_R, -2,
JMPLE, 107, //then_13
JMP, 133, //endif_13
//107, then_13:
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
JMP, 99, //loop_4
//133, endif_13:
RET,
//134, endproc_12:
LOAD_DEST_R, 6,
LOAD_SRC1_R, 1,
MOV,
LOAD_DEST_R, 7,
LOAD_SRC1_V, 10000000,
MOV,
LOAD_DEST_R, 8,
LOAD_SRC1_R, 4,
MOV,
LOAD_DEST_R, 9,
LOAD_SRC1_R, 4,
MOV,
LOAD_SRC1_V, 10,
CALL, 99, //loop_4
LOAD_SRC2_V, 1,
LOAD_DEST_R, 3,
LOAD_SRC1_R, 3,
ADD,
LOAD_SRC2_V, 10001,
LOAD_SRC1_R, 3,
JMPEQ, 173, //then_14
JMP, 176, //endif_14
//173, then_14:
LOAD_SRC1_R, 4,
PRINT,
//176, endif_14:
//176, endif_15:
LOAD_SRC2_V, 1,
LOAD_DEST_R, 4,
LOAD_SRC1_R, 4,
ADD,
JMP, 66, //while_10
//185, endif_16:
//185, endloop_11:
LOAD_SRC1_R, 3,
PRINT,
