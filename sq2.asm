MOV|RVR, 0, 0,
//3, while1:
JMPLE|RRV, 9, 0, 20, //then5
JMP, 40, //endif5
//9, then5:
MUL|RRR, 1, 0, 0,
MOD|RRV, 2, 1, 2,
MOD|RRV, 3, 1, 5,
JMPEQ|RRV, 31, 2, 0, //then4
JMPEQ|RRV, 31, 3, 0, //then2
JMP, 34, //endif4
//31, then4:
//31, then2:
PRINT|RRR, 0, 1,
//34, endif4:
ADD|RRV, 0, 0, 1,
JMP, 3, //while1
//40, endif5:
