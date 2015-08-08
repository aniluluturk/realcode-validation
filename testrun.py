#testrun.py


from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

import pexpect
import sys
import time
import traceback
import os
import re
import getopt

def readlines(filename):

	tc = open(filename,'rw+')
	ls = tc.readlines()
	instr_arr = [x.split()[1].replace('0x','') for x in ls]
	return instr_arr


def testgen(instr_list):
	
	testb = 'run_test debug NONE (Manual ['
	#testm  = '("4802", "", 2),("4685", "", 2),("4802", "", 2),("4780", "", 2)'
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

def runtest(instr_arr,testno):

	#instr_arr = readlines('tc2.txt')
	#instr_seq = testgen([4802,4685,4802,4780])
	#instr_arr = readlines('tc2.txt')
	instr_seq = testgen(instr_arr)


	#open file for logging
	f = open('log_' + testno + '.txt','wb')

	child = pexpect.spawnu('hol')
	time.sleep(0.3)
	st = child.expect(u'\n>')
	#print(child.before)
	#f.write(child.before + '\n') #dont add to the log for now
	#st = child.expect('>')
	#print(child.before)
	#print("....\n")
	#print(st)

	child.sendline('load "Test";')
	time.sleep(0.3)
	st = child.expect(u'\n>')
	#print(child.before)
	#f.write(child.before + '\n') #dont add to the log for now

	child.sendline('open Test;')
	time.sleep(0.3)
	st = child.expect(u'\n>')
	#print(child.before)
	#f.write(child.before + '\n') #dont add to the log for now

	child.sendline('val () = max_print_depth := 200;')
	time.sleep(0.3)
	st = child.expect(u'\n>')
	#print(child.before)
	#f.write(child.before + '\n') #dont add to the log for now


	child.sendline('val debug = HWTest.connect();')
	time.sleep(0.3)
	st = child.expect(u'\n>')
	#print(child.before)
	#f.write(child.before + '\n')  #dont add to the log for now

	try:
		child.sendline(instr_seq)
		time.sleep(15*len(instr_arr))
		st = child.expect("> ",timeout=80)
                trest = child.before
		print(child.before)
		f.write(child.before + '\n')
		stat = "FAIL"
		if( "All comparisons OK." in trest ):
			stat = "SUCCESS"
		f.write("** TEST RESULT: " + stat + '\n')
	except Exception,err:
		#print("EXCEPT:" + child.readline()+'\n')
		#print(traceback.format_exc())
		#f.write(child.before + '\n')
		#print("************** " + child.readlines())
		print("Error\n")
	
	child.close()
	return trest
	#st = child.readlines()
	#print(st)

def main():
	
	filename='tcs'
	
	'''
	time1 = datetime.datetime.now() 
	sleep(3)
	time2 = datetime.datetime.now()
	timediff = time2 - time1
	print(timediff)
	print(timediff.total_seconds())
	'''

	try:
		myopts, args = getopt.getopt(sys.argv[1:],"f:")
	except getopt.GetoptError as e:
    		print (str(e))
    		print("Usage: %s [-f file_name]" % sys.argv[0])
    		sys.exit(2)
 
	for o, a in myopts:
    		if o == '-f':
			filename=a

	if(filename == ''):
		filename = 'tcs'
	print("filename: " + filename) 	


	ch = pexpect.spawn('openocd -f xmc2go.cfg -l openocd.errors')
	time.sleep(1)

	

	cases = [f for f in os.listdir('.') if re.match(str(filename)+r'[0-9]*\.txt\Z',f)]
	cases.sort()
	num_tests = len(cases)
	succ_tests = 0
	fail_tests = 0
	for case in cases:
		print("===== Running test : " + case + " =======")
		instr_arr = readlines(case)
		res = runtest(instr_arr,case.replace('.txt',''))
		stat = "FAIL"
		if( "All comparisons OK." in res ):
			stat = "SUCCESS"
			succ_tests = succ_tests +1
		else:
			fail_tests = fail_tests +1
		print("===== End of test: " + stat +  " =======")
	
	print("===== OVERVIEW ===============\n")
	print("Number of successful tests: " + str(succ_tests))
	print("Number of failed tests: " + str(fail_tests))

	ch.close()
	return

if __name__ == "__main__":
    main()

