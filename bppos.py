import gdb
o = gdb.execute('file SOMEFILE',to_string=True)
print(o)

bf = open("breaks_source.txt")
breaks = bf.readlines()

bo = open("breaks_loc.txt","w")

for brp in breaks:
	filepos = brp.split()[0]
	l = brp.split()[1]
	o = gdb.execute('break ' + str(filepos),to_string=True)
	print(o)
	k = o.split("at ")[1]
	k = k.split(":")[0]
	print(k)

	bo.write(k + " "+ l+ "\n")

bf.close()
bo.close()
