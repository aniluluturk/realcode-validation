import re
import time
import telnetlib

mem_ops = 'MOVS','MOV','LDR','LDRH','LDRB','LDRSH','LDRSB','LDM','STR','STRH','STRB','STM'
ari_ops = 'ADDS','ADD','ADCS','ADR','SUBS','SUB','SBCS','RSBS','MULS'
com_ops = 'CMP','CMN'
log_ops = 'ANDS','EORS','ORRS','BICS','MVNS','TST'
sys_ops = 'PUSH','POP','SVC','CPSID','CPSIE','MRS','MSR','BKPT','SEV','WFE','WFI','YIELD','NOP','ISB','DMB','DSB'
bra_ops = 'B','BL','BLX','BX'
man_ops = 'SXTH','SXTB','UXTH','UXTB','REV','REV16','REVSH','LSLS','LSRS','RORS','ASRS'

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

for l in ls:
	inst =re.findall('\"(.*?)\"',l)
	hexs = inst[1::2]
	#print hexs
	dic1 = {}
	for h in hexs:
		h = h.split()[0]
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

ou = open('dict_random','w')

print(dic)
print(instr_typelist)
ou.write(str(dic))

ocsv = open('typelist.csv','w')
ocsv.write('Instruction Type,Num. Occurences\n')
ocsv.write('Memory,' + str(instr_typelist[0]) + '\n')
ocsv.write('Arithmetic,' + str(instr_typelist[1]) + '\n')
ocsv.write('Comparison,' + str(instr_typelist[2]) + '\n')
ocsv.write('Logic,' + str(instr_typelist[3]) + '\n')
ocsv.write('System,' + str(instr_typelist[4]) + '\n')
ocsv.write('Branch,' + str(instr_typelist[5]) + '\n')
ocsv.write('Register Manip.,' + str(instr_typelist[6]) + '\n')
ocsv.write('Other,' + str(instr_typelist[7]) + '\n')


