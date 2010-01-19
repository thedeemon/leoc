JMP, 38, //endproc2
//2, fib:
JMPLE|RRV, 34, -1, 2, //then3
SUB|RRV, 1, -1, 1,
MOV|RRR, 2, 1,
CALL|RVR, 2, 3, //fib
SUB|RRV, 1, -1, 2,
MOV|RRR, 3, 1,
CALL|RVR, 2, 4, //fib
ADD|RRR, 0, 2, 3,
MOV|RRR, -1, 0,
RET,
//34, then3:
MOV|RVR, -1, 1,
RET,
//38, endproc2:
MOV|RVR, 0, 5,
CALL|RVR, 2, 1, //fib
PRINT|RRR, 0, 0,
