MOV|RVR, 0, 0,
//3, while1:
JMPLE|RRV, 9, 0, 20, //then5
JMP, 44, //endif5
//9, then5:
MUL|RRR, 1, 0, 0,
MOD|RRV, 2, 1, 2,
MOD|RRV, 3, 1, 5,
JMPEQ|RRV, 35, 2, 0, //then4
JMPEQ|RRV, 31, 3, 0, //then3
JMP, 33, //endif3
//31, then3:
JMP, 35, //then2
//33, endif3:
JMP, 38, //endif4
//35, then4:
//35, then2:
PRINT|RRR, 0, 1,
//38, endif4:
ADD|RRV, 0, 0, 1,
JMP, 3, //while1
//44, endif5:
