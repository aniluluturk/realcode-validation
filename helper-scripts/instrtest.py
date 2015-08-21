
testb = 'run_test debug NONE (Manual ['
testm  = '("4802", "", 2),("4685", "", 2),("4802", "", 2),("4780", "", 2)'
teste = ']) NONE (Basic Breakpoint) [];'


instrb = '("'
instre = '", "", 2),'
instre0 = '", "", 2)'


instr_list = [4802,4865,4802,4870]
instr_str = ""
for ins in instr_list:
    instr_str = instr_str + instrb + str(ins) + instre
instr_str = instr_str[0:-1]

test_str = testb + instr_str + teste

print test_str
