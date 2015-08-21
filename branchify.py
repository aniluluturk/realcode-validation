import os,sys,re

files= sys.argv[1:]
logo = open('branchifylog','w')

if(files[0] == '-f'):
	ls = open(files[1],'r')
	files = ls.readlines()

k =0 
for fil in files:
	fi = open(fil.replace("\n","").replace("log_",""),"r")
	ls = fi.readlines()
	ls = [x.split() for x in ls]
	for j in range(len(ls)-1):
		curl = ls[j]
		print "curl:" + str(curl)
		#deleted cond int(ls[j+1][0],16) != int(ls[j][0],16)+2
		if(curl[1] != 'BLX' and re.findall(r'B[A-Z][A-Z]',curl[2]) != [] ):
			if(ls[j+1][0] ==ls[j][3] ):
				ls[j].append("***")
				print "appended!"
			elif( int(ls[j+1][0],16) == int(ls[j][0],16)+2 ):
				ls[j].append("???")
				print "Dappended"

	for j in range(len(ls)):
		ls[j] = "\t".join(ls[j])

	#wd = os.path.dirname(fil)
	wd = "./" # for now
	fo = open(wd+"fcs"+str(k)+".txt","w")
	fo.write("\n".join(ls))
	print "Org file:" + fil + "\n new file: " +  wd+"fcs"+str(k)+".txt"
	logo.write("Org file:" + fil + "\n new file: " +  wd+"fcs"+str(k)+".txt\n")
	k = k+1

