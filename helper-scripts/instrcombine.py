#combine.py, combines available dictionaries into one, and generates csv file for latex

#f = open('dict_random')

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
			dic_all[key] = dic[key]
		else:
			dic_all[key] = dic[key] + dic_all[key]

print(dic_all)
ou = open('dict_real_all','w')

ou.write(str(dic_all))

csv = open("table_all.csv","w")
for key in dic_all:
	csv.write(str(key) + ',' + str(dic_all[key])+'\n')
csv.close()


					
