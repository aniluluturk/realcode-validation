import sys,os,re

filelist = sys.argv[1:]

mem_ops = 'MOVS','MOV','LDR','LDRH','LDRB','LDRSH','LDRSB','LDM','STR','STRH','STRB','STM'
ari_ops = 'ADDS','ADD','ADC','ADCS','ADR','SUBS','SUB','SBCS','RSBS','MULS','MUL','RSB','SBC'
com_ops = 'CMP','CMN'
log_ops = 'ANDS','EORS','ORRS','BICS','MVNS','TST','EOR','MVN','ORR'
sys_ops = 'PUSH','POP','SVC','CPSID','CPSIE','MRS','MSR','BKPT','SEV','WFE','WFI','YIELD','NOP','ISB','DMB','DSB'
bra_ops = 'B','BL','BLX','BX','BCC','BCS','BEQ','BIC','BLS','BNE','BPL','BGE','BGT','BHI','BLE','BLT','BMI','BVC','BVS'
man_ops = 'SXTH','SXTB','UXTH','UXTB','REV','REV16','REVSH','LSLS','LSRS','RORS','ASR','ASRS','LSL','LSR','ROR'

dic = {}

instr_list = []
instr_typelist = [0,0,0,0,0,0,0,0]


for file in filelist:
	lines = open(file,'r')
	instr_list = []
	for line in lines:
		h = line.split()[2]

		instr_list.append(h)

		if(h in dic):
			dic[h] = dic[h] +1
		else:
			dic[h] = 1

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

	#print dic1

ou = open('dict_real','w')

print(dic)
print(instr_typelist)
ou.write(str(dic))

ocsv = open('typelist.csv','w')
ocsv.write('Instruction Type,Num. Occurences\n')
ocsv.write('(M)Memory,' + str(instr_typelist[0]) + '\n')
ocsv.write('(A)Arithmetic,' + str(instr_typelist[1]) + '\n')
ocsv.write('(C)Comparison,' + str(instr_typelist[2]) + '\n')
ocsv.write('(L)Logic,' + str(instr_typelist[3]) + '\n')
ocsv.write('(S)System,' + str(instr_typelist[4]) + '\n')
ocsv.write('(B)Branch,' + str(instr_typelist[5]) + '\n')
ocsv.write('(R)Register Manip.,' + str(instr_typelist[6]) + '\n')
ocsv.write('(O)Other,' + str(instr_typelist[7]) + '\n')

		
