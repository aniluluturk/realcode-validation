import telnetlib
import re
from time import sleep
import sys, getopt,os
import datetime



def singlestep_l(bp_loc,num_lines):

	port = '4444'
	host = 'localhost'
	f = open('log' + str(bp_loc) + '.txt','wb')

	tn = telnetlib.Telnet(host,port)
	tn.write('halt\n')
	sleep(0.3)
	d = tn.read_very_eager()
	i = 0
	if(bp_loc != 'reset'):
		tn.write('bp ' + bp_loc + ' 2 hw\n')
		sleep(0.2)
		d = tn.read_very_eager()
		print('**' +d)
	#tn = telnetlib.Telnet(host,port)
	if(bp_loc != 'reset'):
		tn.write('reset run\n')
		sleep(5)
		d = tn.read_very_eager()  #we need to check whether we stopped at a breakpoint here
	else:
		tn.write('reset halt\n')
		sleep(5)
		d = tn.read_very_eager()  
	while i< num_lines:
		try:
			tn.write('step\n')
	        	sleep(0.5)
        		d = tn.read_very_eager()
        		print('--'+d)
			tn.write('reg pc\n')
        		sleep(0.3)
        		a = tn.read_very_eager()
        		#a =a.replace('\r\n','')
        		#b = a.split(':')
        		#print(b)
        		#r = b[1].strip()
        		print(a)
        		k =re.findall(r'0x[0-9A-F]+',a,re.I)
        		print(k)
        		r = k[0]
        		#print('here is :' + 'reg ' + r + '\n')	
        		tn.write('arm disassemble ' + r + '\n')
        		sleep(0.3)
        		a = tn.read_very_eager()
			
			#get the second line of the output
			a = a.split('\n')[1]
		
        		print("res:"+a)
        		f.write(a+'\n')
        		i = i+1
			print("num_instructions: " + str(i))
		except:
			print("Error at step:" + str(i))
	
	tn.write('rbp ' + bp_loc + '\n')	

	tn.write('exit\n')
	print(tn.read_all())
	f.close()


def singlestep_t(total_time):

	port = '4444'
	host = 'localhost'
	f = open('log.txt','wb')

	tn = telnetlib.Telnet(host,port)
	tn.write('reset halt\n')
	sleep(0.3)
	d = tn.read_very_eager()
	#tn.write('reg pc 0x10001018\n')
	i = 0
	sleep(0.1)
	d = tn.read_very_eager()
	print('**' +d)


	time1 = datetime.datetime.now()

	while True:
		tn.write('step\n')
        	sleep(0.5)
        	d = tn.read_very_eager()
        	print('--'+d)
		tn.write('reg pc\n')
        	sleep(0.1)
        	a = tn.read_very_eager()
        	#a =a.replace('\r\n','')
        	#b = a.split(':')
        	#print(b)
        	#r = b[1].strip()
        	print(a)
        	k =re.findall(r'0x[0-9A-F]+',a,re.I)
        	print(k)
        	r = k[0]
        	#print('here is :' + 'reg ' + r + '\n')	
        	tn.write('arm disassemble ' + r + '\n')
        	sleep(0.1)
        	a = tn.read_very_eager()
	
		#get the second line of the output
		#a = a.split('\n')[1]
	
        	print("res:"+a)
        	f.write('res'+a+'\n')
        	i = i+1
	
		time2 = datetime.datetime.now()
		timediff = time2 - time1
		#print(timediff)
		print(timediff.total_seconds())
		spent_time = timediff.total_seconds()

		if(spent_time >= total_time):
			break
	
	tn.write('exit\n')
	print(tn.read_all())
	f.close()

def main():
	time=''
	lines=''
	
	'''
	time1 = datetime.datetime.now() 
	sleep(3)
	time2 = datetime.datetime.now()
	timediff = time2 - time1
	print(timediff)
	print(timediff.total_seconds())
	'''
	brfile = "breaks.txt"
	if(len(sys.argv) >1):
		brfile = "breaks_loc.txt"

		temp = open("bppos.py")
		ls = temp.readlines()
		ls = "".join(ls)

		#print ls
		ls = ls.replace("SOMEFILE",sys.argv[1])
		print ls
		tempout = open("bppos1.py","w")
		tempout.write(ls)
		tempout.close()

		#exit(0)

		os.system("gdb -q -x bppos1.py --batch")
		#sleep(3)
		#ch = open("breaks_loc.txt")
		#print(ch.readlines())
		

	br_list = []
	print(brfile)
	brfile = open(brfile,"r")
	breaks = brfile.readlines()
	for br in breaks:
		br_list.append(br.split())
		print(br + "\n")
	print br_list
	#return

	for br in br_list:
		singlestep_l(br[0],int(br[1]))

	return

	try:
		myopts, args = getopt.getopt(sys.argv[1:],"l:t:")
	except getopt.GetoptError as e:
    		print (str(e))
    		print("Usage: %s [-l number_lines] [-t total_runtime]" % sys.argv[0])
    		sys.exit(2)
 
	for o, a in myopts:
    		if o == '-l':
			lines=a
		elif o == '-t':
			time=a

	print("l: " + lines + " t: " + time) 

	#sys.exit()
	if(lines !=""):
		singlestep_l(int(lines))
	elif(time !=""):
		singlestep_t(int(time))
	else:
		print("Please supply argument for time or line")



if __name__ == "__main__":
    main()
