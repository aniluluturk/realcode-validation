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
import argparse

branch_disable = False
cdir = '.'

def readlines(filename):

	branches = []
	tc = open(cdir+'/'+filename,'rw+')
	ls = tc.readlines()
	instr_arr = [x.split()[1].replace('0x','') for x in ls]
	for l in ls:
		if("***" in l and not branch_disable): 
			branches.append(0)
		if("???" in l and not branch_disable): 
			branches.append(1)
		else:
			branches.append(0)
	#branches.append(0)
	#branches = branches[1:]
	return [instr_arr,branches]


def testgen(instr_list,branch_list):
	
	testb = 'run_test debug NONE (Manual ['
	#testm  = '("4802", "", 2),("4685", "", 2),("4802", "", 2),("4780", "", 2)'
	teste = ''
	if(branch_disable == False):
		teste = ']) (SOME ' + str(branch_list) + ') (Basic Breakpoint) [];'
	else:
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

def runtest(instr_arr,testno,branch_arr):

	#instr_arr = readlines('tc2.txt')
	#instr_seq = testgen([4802,4685,4802,4780])
	#instr_arr = readlines('tc2.txt')
	instr_seq = testgen(instr_arr,branch_arr)

	if(os.path.exists(cdir+'/'+'log_' + testno + '.txt')):
		f = open(cdir+'/'+'log_' + testno + '.txt','r')
		fc = f.readlines()
		return("\n".join(fc))

	#open file for logging
	f = open(cdir+'/'+'log_' + testno + '.txt','wb')

	child = pexpect.spawnu('hol',timeout=None)
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

	trest = 'Empty Log'
	try:
		child.sendline(instr_seq)
		time.sleep(1*len(instr_arr))
		#st = child.expect("> ",timeout=80)
		child.sendcontrol("d")
		child.expect(pexpect.EOF)
                trest = child.before
		print(child.before)
		f.write(child.before + '\n')
		stat = "FAIL"
		if( "All comparisons OK." in trest ):
			stat = "SUCCESS"
		f.write("** TEST RESULT: " + stat + '\n')
	except KeyboardInterrupt:
    		# continue to next test
    		print("Test got stuck, ended with SIGINT")
	except Exception,err:
		#print("EXCEPT:" + child.readline()+'\n')
		print(traceback.format_exc())
		#f.write(child.before + '\n')
		#print("************** " + child.readlines())
		print("Child Process Error (HOL)\n")
	
	child.close()
	return trest
	#st = child.readlines()
	#print(st)

def main():
	#time check
	global branch_disable
	global cdir

	start_time = time.time()

	filename='tcs'
	#cdir = '.'
	
	'''
	time1 = datetime.datetime.now() 
	sleep(3)
	time2 = datetime.datetime.now()
	timediff = time2 - time1
	print(timediff)
	print(timediff.total_seconds())
	'''

	parser = argparse.ArgumentParser(description='Arg description')#,allow_abbrev = False)
	parser.add_argument("--nobranch",help='disable branch handling',action='store_true',dest='nobranch')
	parser.add_argument("-f",help='change file prefix to be used')
	parser.add_argument("-d",help='change working directory')

	args= parser.parse_args()
	print(str(args.nobranch) + "!")
	branch_disable = args.nobranch
	print('nobranch:' + str(branch_disable))
	print(args.f)
	if(args.f != None):
		filename = args.f
	if(args.d != None):
		cdir = args.d
	print(filename)
	print(cdir)

	'''
	return

	try:
		myopts, args = getopt.getopt(sys.argv[1:],"f:")
	except getopt.GetoptError as e:
    		print (str(e))
    		print("Usage: %s [-f file_name]" % sys.argv[0])
    		sys.exit(2)
 
	for o, a in myopts:
    		if o == '-f':
			filename=a
		#if o == '-nobranch':
		#	nobranch=True
		#	print("NOBRANCH")
	return

	if(filename == ''):
		filename = 'tcs'
	print("filename: " + filename) 	
	'''

	ch = pexpect.spawn('openocd -f xmc2go.cfg -l openocd.errors')
	time.sleep(1)

	

	cases = [f for f in os.listdir(cdir) if re.match(str(filename)+r'[0-9]*\.txt\Z',f)]
	cases.sort()
	num_tests = len(cases)
	succ_tests = 0
	fail_tests = 0
	for case in cases:
		print("===== Running test : " + case + " =======")
		instr_arr = readlines(case)
		branch_arr = instr_arr[1]
		instr_arr = instr_arr[0]

		res = runtest(instr_arr,case.replace('.txt',''),branch_arr)
		stat = "FAIL"
		if( "All comparisons OK." in res ):
			stat = "SUCCESS"
			succ_tests = succ_tests +1
		elif("SMT_UNSAT" in res.upper()):
			stat = "Fail - SMT Unsat"
			fail_tests = fail_tests +1
		elif("MISMATCH" in res.upper()):
			stat = "Fail - Comparison Mismatch"
			fail_tests = fail_tests +1
		elif("IMPOSS" in res.upper()):
			stat = "Fail - Imposs. Comb."
			fail_tests = fail_tests +1
		elif("SMT_UNKNOWN" in res.upper()):
			stat = "Fail - SMT Unkown"
			fail_tests = fail_tests +1
		elif("MISCEXN" in res):
			stat = "Fail - Misc. Exn."
			fail_tests = fail_tests +1
		else:
			stat = "Fail - Other"
			fail_tests = fail_tests +1
		print("===== End of test: " + stat +  " =======")
	
	print("===== OVERVIEW ===============\n")
	print("Number of successful tests: " + str(succ_tests))
	print("Number of failed tests: " + str(fail_tests))
	
	elapsed_time = time.time() - start_time
	print("Elapsed time: " + str(elapsed_time) + " seconds")

	ch.close()
	return

if __name__ == "__main__":
    main()

