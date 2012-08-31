cmds = [
['INC', 0],#
['ADD', 1], #    //d, a1, a2 : d = a1 + a2    b
['MUL', 2],#   (2  << CMDSHIFT)    //d, a1, a2 : d = a1 * a2		c
['MOD', 3],#  << CMDSHIFT)    //d, a1, a2 : d = a1 % a2		d
['SUB', 4],#   (4  << CMDSHIFT)    //d, a1, a2 : d = a1 - a2		e
['DIV', 5],#   (5  << CMDSHIFT)    //d, a1, a2 : d = a1 / a2		f
['XOR', 6],#  << CMDSHIFT)    //d, a1, a2 : d = a1 ^ a2		g
['MOV', 7],#  << CMDSHIFT)    //d, a1 : d = a1				h
['MOVB', 8],#  << CMDSHIFT)    //d, a1 : d = a1				i
['JMPLE', 9],#  << CMDSHIFT)    //addr, a1, a2 : if a1 < a2  ip = addr		j
['JMPEQ', 10],# << CMDSHIFT)    //addr, a1, a2 : if a1 == a2 ip = addr		k
['JMP', 11],# << CMDSHIFT)    //addr : ip = addr							l
['PRINT', 12],# << CMDSHIFT)    //x, a1 : print a1							m
['NEW', 13],# << CMDSHIFT)    //d, a1 : d = malloc(a)						n
['CALL', 14],# << CMDSHIFT)    //addr, a1 : fp += a1, call addr				o
['RET', 15],# << CMDSHIFT)    //: return from call							p
['PRCHAR', 16],#
['INC4', 17]
]

@dests = [
 ['|DR', 'frame[fp + code[ip+1]]'],
 ['|DP', '*(int*)frame[fp + code[ip+1]]'],
 ['|DA', 'source1'],
 ['|DB', 'source2']
]

@destsb = [
 ['|DR', 'frame[fp + code[ip+1]]'],
 ['|DP', '*(BYTE*)frame[fp + code[ip+1]]'],
 ['|DA', 'source1'],
 ['|DB', 'source2']
]


@sources1 = [
 ['|AR', 'frame[fp + code[ip+2]]'],
 ['|AP', '*(int*)frame[fp + code[ip+2]]'],
 ['|AV', 'code[ip+2]'],
 ['|AS', 'source1']
]

@sources2 = [
 ['|BR', 'frame[fp + code[ip+3]]'],
 ['|BP', '*(int*)frame[fp + code[ip+3]]'],
 ['|BV', 'code[ip+3]'],
 ['|BS', 'source2']
]

@sources1b = [
 ['|AR', '(BYTE)frame[fp + code[ip+2]]'],
 ['|AP', '*(BYTE*)frame[fp + code[ip+2]]'],
 ['|AV', 'code[ip+2]'],
 ['|AS', '(BYTE)source1']
]


def gen(line)
  dests = line.include?('LOAD_D;') ? @dests : 
    (line.include?('LOAD_DB') ? @destsb :  [['','']])
  sources1 = line.include?('LOAD_A1;') ? @sources1 : 
    (line.include?('LOAD_A1B') ? @sources1b :  [['','']])
  sources2 = line.include?('LOAD_A2') ? @sources2 : [['','']]
  for d in dests do
    for a1 in sources1 do
      for a2 in sources2 do
        ln = line.gsub('%mods', d[0]+a1[0]+a2[0]).
              gsub('LOAD_A1;','').gsub('source1', a1[1]).
              gsub('LOAD_A1B;','').
              gsub('LOAD_A2;','').gsub('source2', a2[1]).
              gsub('LOAD_D;', '').gsub('LOAD_DB;', '').gsub('*dest', d[1])              
        print ln
      end
    end
  end
end #def
gen("case ADD%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 + source2; ip += 4; break;\n")
gen("case MUL%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 * source2; ip += 4; break;\n")
gen("case MOD%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 % source2; ip += 4; break;\n")
gen("case SUB%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 - source2; ip += 4; break;\n")
gen("case DIV%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 / source2; ip += 4; break;\n")
gen("case XOR%mods: LOAD_D; LOAD_A1; LOAD_A2; *dest = source1 ^ source2; ip += 4; break;\n")
gen("case MOV%mods: LOAD_D; LOAD_A1;  *dest = source1;           ip += 3; break;\n")
gen("case MOVB%mods: LOAD_DB; LOAD_A1B; *dest = source1;   ip += 3; break;\n")
gen("case JMPLE%mods: LOAD_A1; LOAD_A2; if (source1 < source2) ip = code[ip+1]; else ip += 4; break;\n")
gen("case JMPEQ%mods: LOAD_A1; LOAD_A2; if (source1 == source2) ip = code[ip+1]; else ip += 4; break;\n")
gen("case JMP%mods: ip = code[ip+1]; break;\n")
gen("case PRINT%mods: LOAD_A1; printf(\"%d \", source1);   ip += 3; break;\n")
gen("case PRCHAR%mods: LOAD_A1; printf(\"%c\", source1);   ip += 3; break;\n")
gen("case NEW%mods: LOAD_D; LOAD_A1; *dest = (int)&heap[hp]; hp += source1; ip += 3; break;\n")
gen("case CALL%mods: 
				LOAD_A1;
				callstack[csp].fp = fp;
				callstack[csp].hp = hp;
				callstack[csp].ip = ip + 3;
				csp++;
				fp += source1;
				ip = code[ip+1];
				break;\n")
gen("case RET:
				if (csp<1) return;
				csp--;
				fp = callstack[csp].fp;
				hp = callstack[csp].hp;
				ip = callstack[csp].ip;
				break;	\n")
gen("case INC:  frame[fp + code[ip+1]]++;    ip += 4; break;\n")
gen("case INC4: frame[fp + code[ip+1]] += 4; ip += 4; break;\n")
