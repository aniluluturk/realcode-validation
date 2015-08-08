import random
import sys
import getopt
import os
import re
import time

number=''
lines=''
logfile = 'log.txt'

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
print dic

length = len(lines)
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



	
