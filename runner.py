from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

import pexpect
import sys
import time
import traceback

def readlines(filename):

	tc = open(filename,'rw+')
	ls = tc.readlines()
	instr_arr = [x.split()[1].replace('0x','') for x in ls]
	return instr_arr


def testgen(instr_list):
	
	testb = 'run_test debug NONE (Manual ['
	testm  = '("4802", "", 2),("4685", "", 2),("4802", "", 2),("4780", "", 2)'
	teste = ']) NONE (Basic Breakpoint) [];'
	
	
	instrb = '("'
	instre = '", "", 2),'
	instre0 = '", "", 2)'

	
	instr_str = ""
	for ins in instr_list:
	    instr_str = instr_str + instrb + str(ins) + instre
	instr_str = instr_str[0:-1]
	
	test_str = testb + instr_str + teste
	
	print(test_str)
	return test_str

#instr_arr = readlines('tc2.txt')
#instr_seq = testgen([4802,4685,4802,4780])
instr_arr = readlines('tc2.txt')
instr_seq = testgen(instr_arr)


#open file for logging
f = open('log.txt','wb')


child = pexpect.spawnu('hol')
time.sleep(0.3)
st = child.expect(u'\n>')
print(child.before)
f.write(child.before + '\n')
#st = child.expect('>')
#print(child.before)
#print("....\n")
#print(st)

child.sendline('load "Test";')
time.sleep(0.3)
st = child.expect(u'\n>')
print(child.before)
f.write(child.before + '\n')

child.sendline('open Test;')
time.sleep(0.3)
st = child.expect(u'\n>')
print(child.before)
f.write(child.before + '\n')

child.sendline('val () = max_print_depth := 200;')
time.sleep(0.3)
st = child.expect(u'\n>')
print(child.before)
f.write(child.before + '\n')


child.sendline('val debug = HWTest.connect();')
time.sleep(0.3)
st = child.expect(u'\n>')
print(child.before)
f.write(child.before + '\n')

try:
	child.sendline(instr_seq)
	time.sleep(15*len(instr_arr))
	st = child.expect(u'\n>')
	print(child.before)
	f.write(child.before + '\n')
except Exception,err:
	print("EXCEPT:" + child.readline()+'\n')
	print(traceback.format_exc())
	f.write(child.before + '\n')

child.close()
#st = child.readlines()
#print(st)
