MOV|RVR, 0, 1,
//3:
JMPLE|RRV, 8, 0, 21,
RET,
//8:
MOV|RRR, 1, 0,
CALL|RVR, 23, 2,
PRINT|RRR, 0, 1,
ADD|RRV, 0, 0, 1,
JMP, 3,
//23:
JMPLE|RRV, 46, -1, 2,
SUB|RRV, 0, -1, 1,
CALL|RVR, 23, 1,
SUB|RRV, 1, -1, 2,
CALL|RVR, 23, 2,
ADD|RRR, -1, 0, 1,
RET,
//46:
MOV|RVR, -1, 1,
RET,
