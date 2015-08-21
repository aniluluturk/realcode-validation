import re,os,sys

files = sys.argv[1:]
lines = []
sum_numbers = []
sample_lines = []
sample_nums = []

for file in files:
	f = open(file,"r")
	lines = []
	lines = f.readlines()
	sample_lines = lines
	f.close()

	if(sum_numbers == []):
		sum_numbers = [0] * (len(lines)-1)

	sample_nums = []
	for i in range(1,len(lines)):
		line = lines[i]
		print(line)
		num = int(re.findall('\d+',line)[0])
		sample_nums.append(num)
		sum_numbers[i-1] = sum_numbers[i-1] + num


o = open("sum.csv","w")
o.write(sample_lines[0])
for i in range(1,len(sample_lines)):
	o.write(sample_lines[i].replace(str(sample_nums[i-1]),str(sum_numbers[i-1])))

o.close()

	