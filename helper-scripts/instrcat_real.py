import re
import time
import telnetlib
import pexpect

mem_ops = 'MOVS','MOV','LDR','LDRH','LDRB','LDRSH','LDRSB','LDM','STR','STRH','STRB','STM'
ari_ops = 'ADDS','ADD','ADC','ADCS','ADR','SUBS','SUB','SBCS','RSBS','MULS','MUL','RSB','SBC'
com_ops = 'CMP','CMN'
log_ops = 'ANDS','EORS','ORRS','BICS','MVNS','TST','EOR','MVN','ORR'
sys_ops = 'PUSH','POP','SVC','CPSID','CPSIE','MRS','MSR','BKPT','SEV','WFE','WFI','YIELD','NOP','ISB','DMB','DSB'
bra_ops = 'B','BL','BLX','BX','BCC','BCS','BEQ','BIC','BLS','BNE','BPL','BGE','BGT','BHI','BLE','BLT','BMI','BVC','BVS'
man_ops = 'SXTH','SXTB','UXTH','UXTB','REV','REV16','REVSH','LSLS','LSRS','RORS','ASR','ASRS','LSL','LSR','ROR'


ls = []
f = open('m0-paper-tests/Success')
ls1 = f.readlines()
ls.extend(ls1)

f = open('m0-paper-tests/Fail_Imposs_comb')
ls2 = f.readlines()
#ls.extend(ls2)

f = open('m0-paper-tests/Fail_SMT_Unsat')
ls3 = f.readlines()
#ls.extend(ls3)

f = open('m0-paper-tests/Fail_SMT_Unknown')
ls4 = f.readlines()
#ls.extend(ls4)

f = open('m0-paper-tests/Fail_Mismatch')
ls5 = f.readlines()
ls.extend(ls5)

dic = {}

instr_list = []
instr_typelist = [0,0,0,0,0,0,0,0]


child = pexpect.spawnu('hol',timeout=None)
time.sleep(0.3)
st = child.expect(u'\n>')
#print(child.before)
#f.write(child.before + '\n') #dont add to the log for now
#st = child.expect('>')
#print(child.before)
#print("....\n")
#print(st)

child.sendline('load "Target";')
time.sleep(0.3)
st = child.expect(u'\n>')
#print(child.before)
#f.write(child.before + '\n') #dont add to the log for now


for l in ls:
	inst =re.findall('\"(.*?)\"',l)
	hexs = inst[0::2]
	#print hexs
	dic1 = {}
	for h in hexs:
		h = h.split()[0]

		child.sendline('Target.print_disassembled_hex "' + h +  '";')
		#time.sleep(0.3)
		st = child.expect(u'\n>')
		#print(child.before.split())
		print(child.before.split()[2])
		#print(child.before.split()[1].split()[0])
		#f.write(child.before + '\n') #dont add to the log for now
		h = child.before.split()[2].upper()

		#print h
		instr_list.append(h)

		if(h in dic):
			dic[h] = dic[h] +1
		else:
			dic[h] = 1
		if(h in dic1):
			dic1[h] = dic1[h] +1
		else:
			dic1[h] = 1

		print("inst: " + h )

		if(h in mem_ops):
			print("1\n")
			instr_typelist[0] = instr_typelist[0]+1
		elif(h in ari_ops):
			print("2\n")
			instr_typelist[1] = instr_typelist[1]+1
		elif(h in com_ops):
			print("3\n")
			instr_typelist[2] = instr_typelist[2]+1
		elif(h in log_ops):
			print("4\n")
			instr_typelist[3] = instr_typelist[3]+1
		elif(h in sys_ops):
			print("5\n")
			instr_typelist[4] = instr_typelist[4]+1
		elif(h in bra_ops):
			print("6\n")
			instr_typelist[5] = instr_typelist[5]+1
		elif(h in man_ops):
			print("7\n")
			instr_typelist[6] = instr_typelist[6]+1
		else:
			print("no cat, sorry\n")
			instr_typelist[7] = instr_typelist[7]+1

	print dic1

ou = open('dict_random_hex','w')

print(dic)
print(instr_typelist)
ou.write(str(dic))

ocsv = open('typelist_random_hex.csv','w')
ocsv.write('Instruction Type,Num. Occurences\n')
ocsv.write('Memory,' + str(instr_typelist[0]) + '\n')
ocsv.write('Arithmetic,' + str(instr_typelist[1]) + '\n')
ocsv.write('Comparison,' + str(instr_typelist[2]) + '\n')
ocsv.write('Logic,' + str(instr_typelist[3]) + '\n')
ocsv.write('System,' + str(instr_typelist[4]) + '\n')
ocsv.write('Branch,' + str(instr_typelist[5]) + '\n')
ocsv.write('Register Manip.,' + str(instr_typelist[6]) + '\n')
ocsv.write('Other,' + str(instr_typelist[7]) + '\n')


