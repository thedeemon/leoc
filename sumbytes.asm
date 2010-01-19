JMP, 38, //endproc1
//2, sumbytes:
MOV|RVR, 0, 0,
MOV|RVR, 1, 0,
//8, while2:
JMPLE|RRR, 14, 0, -1, //then3
JMP, 34, //endif3
//14, then3:
ADD|RRR, 3, -2, 0,
MOV|RVR, 2, 0,
MOVB|RPR, 2, 3,
ADD|RRR, 1, 1, 2,
ADD|RRV, 0, 0, 1,
JMP, 8, //while2
//34, endif3:
MOV|RRR, -2, 1,
RET,
//38, endproc1:
NEW|RVR, 0, 2,
MOVB|PVR, 0, 33,
ADD|RRV, 1, 0, 1,
MOVB|PVR, 1, 44,
MOV|RRR, 2, 0,
MOV|RVR, 3, 2,
CALL|RVR, 2, 4, //sumbytes
PRINT|RRR, 0, 2,
