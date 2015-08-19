import telnetlib
import re
from time import sleep
import sys, getopt
import datetime

instr_branch = ['B','BX','BLX','BL']

def singlestep_l(num_lines,logfile = 'log.txt'):

	port = '4444'
	host = 'localhost'
	f = open(logfile,'wb')

	tn = telnetlib.Telnet(host,port)
	tn.write('halt\n')
	sleep(0.3)
	d = tn.read_very_eager()
	tn.write('reg pc 0x10001018\n')
	i = 0
	sleep(0.1)
	d = tn.read_very_eager()
	print('**' +d)
	pc = ''
	pcl = '0x0'
	instrl = ''
	branches = []
	while i< num_lines:
		try:
			tn.write('step\n')
        		sleep(0.5)
	        	d = tn.read_very_eager()
        		print('--'+d)
			tn.write('reg pc\n')
        		sleep(0.2)
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
        		sleep(0.2)
        		a = tn.read_very_eager()
			
			#get the second line of the output
			a = a.split('\n')[1]

			lst = a.split()
			print("instrname: " + lst[2])
			if(i!=0 and instrl != 'BLX' and re.findall(r'B[A-Z][A-Z]',instrl) != [] ):
				if(int(r,16) != int(pcl,16)+2):
					print("Branch taken\n")
					f.write('***')
					branches.append(0)
				else:
					print("Branch not taken\n")
					f.write('???')
					branches.append(1)
			pcl = r
			instrl = lst[2]


			if(i!=0):
				f.write('\n')

        		print("res:"+a)
        		f.write(a.replace('\n','').replace('\r',''))

        		i = i+1
			print("num_instructions: " + str(i))
		except Exception,e:
			print("***Read error on instruction no: " + str(i))
			print(str(e))
		
		
	tn.write('exit\n')
	print(tn.read_all())
	print("BRANCHES:" + str(branches))
	f.close()


def singlestep_t(total_time):

	port = '4444'
	host = 'localhost'
	f = open('log.txt','wb')

	tn = telnetlib.Telnet(host,port)
	tn.write('halt\n')
	sleep(0.3)
	d = tn.read_very_eager()
	tn.write('reg pc 0x10001018\n')
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
        	sleep(0.2)
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
	logfile = 'log.txt'
	
	'''
	time1 = datetime.datetime.now() 
	sleep(3)
	time2 = datetime.datetime.now()
	timediff = time2 - time1
	print(timediff)
	print(timediff.total_seconds())
	'''

	try:
		myopts, args = getopt.getopt(sys.argv[1:],"f:l:t")
	except getopt.GetoptError as e:
    		print (str(e))
    		print("Usage: %s [-l number_lines] [-t total_runtime]" % sys.argv[0])
    		sys.exit(2)
 
	for o, a in myopts:
    		if o == '-l':
			lines=a
		elif o == '-t':
			time=a
		elif o == '-f':
			print("LOGFILE: " + logfile + "," + a + ",")
			logfile=a

	print("l: " + lines + " t: " + time + " f: " + logfile) 

	#sys.exit()
	if(lines !=""):
		singlestep_l(int(lines),logfile)
	elif(time !=""):
		singlestep_t(int(time))
	else:
		print("Please supply argument for time or line")
		return

	f = open(logfile)
	lines = f.readlines()
	
	dic = {}
	for l1 in lines:
		l = l1.split()
		print(l)
		if(l[2] in dic):
			dic[l[2]] = dic[l[2]] +1
		else:
			dic[l[2]] = 1
	print dic


if __name__ == "__main__":
    main()
