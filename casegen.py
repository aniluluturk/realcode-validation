import random
import sys
import getopt
import os
import re
import time

number=''
lines=''
logfile = 'log.txt'


instr_typelist = [0,0,0,0,0,0,0,0]


mem_ops = 'MOVS','MOV','LDR','LDRH','LDRB','LDRSH','LDRSB','LDM','STR','STRH','STRB','STM'
ari_ops = 'ADDS','ADD','ADCS','ADR','SUBS','SUB','SBCS','RSBS','MULS'
com_ops = 'CMP','CMN'
log_ops = 'ANDS','EORS','ORRS','BICS','MVNS','TST'
sys_ops = 'PUSH','POP','SVC','CPSID','CPSIE','MRS','MSR','BKPT','SEV','WFE','WFI','YIELD','NOP','ISB','DMB','DSB'
bra_ops = 'B','BL','BLX','BX'
man_ops = 'SXTH','SXTB','UXTH','UXTB','REV','REV16','REVSH','LSLS','LSRS','RORS','ASRS'


try:
	myopts, args = getopt.getopt(sys.argv[1:],"f:l:n:")
except getopt.GetoptError as e:
	print (str(e))
    	print("Usage: %s [-l number_instructions] [-t number_testcases]" % sys.argv[0])
    	sys.exit(2)
 
seq_length = 5

for o, a in myopts:
	if o == '-l':
		lines=a
	elif o == '-n':
		number=a
	elif o == '-f':
		logfile=a

if(lines != ''):
	seq_length = int(lines)

print("l: " + lines + " n: " + number) 

f = open(logfile)
lines = f.readlines()

dic = {}
for l1 in lines:
	l = l1.split()
	if(l[2] in dic):
		dic[l[2]] = dic[l[2]] +1
	else:
		dic[l[2]] = 1

	h = l[2]
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


print dic

length = len(lines)
print("***LENGTH: " + str(length))
num_case = length/float(seq_length)/2
if(number != ''):
	num_case = int(number)

f.close()

cases = []
for i in range(0,(int(num_case)+1)):
	pos_start = random.randint(0,length-1)
	pos_end = pos_start + seq_length
	print("s:" + str(pos_start) + ",e:" + str(pos_end))
	if(length < pos_start):
		pos_end = length
	cases.append(lines[pos_start:pos_end])

print("cases:"+str(cases))

files = [f for f in os.listdir('.') if re.match(r'tcs[0-9]*\.txt\Z',f)]
for file in files:
	os.remove(file)
time.sleep(0.2)
i =0

for ci in range(len(cases)):
	out = open("tcs" + str(ci) + ".txt","w")
	dic = {}
	for l1 in cases[ci]:
		l = l1.split()
		if(l[2] in dic):
			dic[l[2]] = dic[l[2]] +1
		else:
			dic[l[2]] = 1

	print dic
	out.write("".join(cases[ci]))
	out.close()
	print("written case " + str(ci)) 

ocsv = open('typelist' + logfile + '.csv','w')
ocsv.write('Instruction Type,Num. Occurences\n')
ocsv.write('Memory,' + str(instr_typelist[0]) + '\n')
ocsv.write('Arithmetic,' + str(instr_typelist[1]) + '\n')
ocsv.write('Comparison,' + str(instr_typelist[2]) + '\n')
ocsv.write('Logic,' + str(instr_typelist[3]) + '\n')
ocsv.write('System,' + str(instr_typelist[4]) + '\n')
ocsv.write('Branch,' + str(instr_typelist[5]) + '\n')
ocsv.write('Register Manip.,' + str(instr_typelist[6]) + '\n')
ocsv.write('Other,' + str(instr_typelist[7]) + '\n')

	
