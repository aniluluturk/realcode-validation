#combine.py, combines available dictionaries into one, and generates csv file for latex

#f = open('dict_random')


mem_ops = 'MOVS','MOV','LDR','LDRH','LDRB','LDRSH','LDRSB','LDM','STR','STRH','STRB','STM'
ari_ops = 'ADDS','ADD','ADC','ADCS','ADR','SUBS','SUB','SBCS','RSBS','MULS','MUL','RSB','SBC'
com_ops = 'CMP','CMN'
log_ops = 'ANDS','EORS','ORRS','BICS','MVNS','TST','EOR','MVN','ORR'
sys_ops = 'PUSH','POP','SVC','CPSID','CPSIE','MRS','MSR','BKPT','SEV','WFE','WFI','YIELD','NOP','ISB','DMB','DSB'
bra_ops = 'B','BL','BLX','BX','BCC','BCS','BEQ','BIC','BLS','BNE','BPL','BGE','BGT','BHI','BLE','BLT','BMI','BVC','BVS'
man_ops = 'SXTH','SXTB','UXTH','UXTB','REV','REV16','REVSH','LSLS','LSRS','RORS','ASR','ASRS','LSL','LSR','ROR'


import os,sys
path = '.'
#files = []
#for i in os.listdir(path):
#    if os.path.isfile(os.path.join(path,i)) and i.startswith('typelist') and not i.endswith('~'):
#        files.append(i)
files = sys.argv[1:]

print(files)

dic_all = {}
print(dic_all)
for f in files:
	f = open(f)
	lines = f.readlines()
	dic = {}
	line = lines[0]
	if(line!= ''):
		dic = eval(line)
	for key in dic:
		if(key not in dic_all):
			dic_all[key] = str(dic[key])
		else:
			dic_all[key] = str(dic_all[key]) + "," + str(dic[key])

for key in dic_all:
	dic_all[key] = ''

for f in files:
	f = open(f)
	lines = f.readlines()
	dic = {}
	line = lines[0]
	if(line!= ''):
		dic = eval(line)
	for key in dic:
		#if(dic_all[key] != ''):
			dic_all[key] = str(dic_all[key]) + str(dic[key])

	for key in dic_all:
		if(key not in dic):
			dic_all[key] = str(dic_all[key]) +"0"
		dic_all[key] = str(dic_all[key]) +","

print(dic_all)
ou = open('dict_nocomb','w')

ou.write(str(dic_all))

csv1 = open("tablenocomb1.csv","w")
csv2 = open("tablenocomb2.csv","w")

csv1.write("Instr. Name, Occur.(Random),Occur.(Real),Type\n")
csv2.write("Instr. Name, Occur.(Random),Occur.(Real),Type\n")
keylist = [key for key in dic_all]
keylist.sort() 
nonempty = 0.0
nonemptyr = 0.0


for key in dic_all:
	h= str(key)
	if(h in mem_ops):
		#print("1\n")
		dic_all[key] = dic_all[key]+'M'
	elif(h in ari_ops):
		#print("2\n")
		dic_all[key] = dic_all[key]+'A'
	elif(h in com_ops):
		#print("3\n")
		dic_all[key] = dic_all[key]+'C'
	elif(h in log_ops):
		#print("4\n")
		dic_all[key] = dic_all[key]+'L'
	elif(h in sys_ops):
		#print("5\n")
		dic_all[key] = dic_all[key]+'S'
	elif(h in bra_ops):
		#print("6\n")
		dic_all[key] = dic_all[key]+'B'
	elif(h in man_ops):
		#print("7\n")
		dic_all[key] = dic_all[key]+'R'
	else:
		#print("no cat, sorry\n")
		dic_all[key] = dic_all[key]+'O'

#for key in dic_all:
for i in range(len(keylist)):
	
	key = keylist[i]
	if(dic_all[key].split(",")[1]!='0'):
		nonempty = nonempty+1
		#print(str(i)+",")
	if(dic_all[key].split(",")[0]!='0'):
		nonemptyr = nonemptyr+1
		
	if(i < len(keylist)/2):
		csv1.write(str(key) + ',' + str(dic_all[key])+'\n')
	else:
		csv2.write(str(key) + ',' + str(dic_all[key])+'\n')


print( "Coverage rate -real:" + str(nonempty/len(keylist)))
print( "Coverage rate - random:" + str(nonemptyr/len(keylist)))
csv1.close()
csv2.close()

#print( "Success rate:" + str((nonempty/len(keylist)))

