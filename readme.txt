This document outlines the usage and I/O specifications of scripts provided within the project for automated testing/validation process

---------- INSTALL ---------------------------------------

Requirements - polyml, hol4 (hol), python, openocd
Python library requirements:  pexpect, gdb, telnetlib  

You only need to run holmake for setting up HOL scripts of Campbell's repository.


--------- RUN EXAMPLE TEST CASES ---------------------------

1. First, one should run openocd with necessary config files, specific to that processor

for xmc1100:
% openocd -f xmc2go.cfg 
for stm32f0
% openocd -f stm32f0discovery.cfg

Note: cfg files can be found in openocd's tcl/boards directory

2. One can start running few of the tests by calling testrun.py with explicitly giving directory names to it

% python testrun.py -d testcases/xmc-logeas-5 --nobranch

this will run all the testcases in xmc-logeas
the results will be generated in the same directoty with prefix log_tcs*

--------- SAMPLE USAGE -----------------------------------

1. since devices are separate for using the python scripts, one needs to load openocd separetely

for xmc1100:
% openocd -f xmc2go.cfg 
for stm32f0
% openocd -f stm32f0discovery.cfg

Note: cfg files can be found in openocd's tcl/boards directory

2. generate execution traces.

for xmc, one can call
% python exec1.py -l 20

for stm one can call
% python exec2.py -l 20

this will generate a log.txt file with 20 instructions listed within

3. generate test cases

% python casegen.py -l 5 -n 4

this will generate 4 test cases with maximum (ideally) 5 instructions each.
the output files will have names tcs0.txt, tcs1.txt ....

4. run the tests

% python testrun.py --nobranch

this will run the tests on HOL4 without supplying any branch choices explicitly. All tests with prefix tcs will be run
their results will be displayed as an overview afterwards,

to see the results of the run, consult log_tcs*.txt files generated for every tcs*.txt test case.  


---------- FILES -----------------------------------------


run.sh

-A simple bash script running exec*.py casegen.py and testrun.py consecutively. 
-It accepts command line arguments to be passed to python file before running the scripts.
-It is very crude, and in case of a python,openocd or bash related problem, it might fail to work properly.
	Using python files separately is encouraged.

exec1.py

-Works only on XMC1100, it has been specifically written due to the "reset halt" problem of the processor
-Generates execution traces starting from reset vector
-It can be called with arguments "-l length_of_trace" or "-t timespent_on_trace"
-Resulting log file is named log.txt, although the name of the file can be changed with command line argument "-f filename"

exec2.py

-Should work on any processor with "reset halt" support on openocd
-Generates execution traces starting from reset vector
-It can be called with arguments "-l length_of_trace" or "-t timespent_on_trace"
-Resulting log file is named log.txt, although the name of the file can be changed with command line argument "-f filename"

exec3.py [New]

-Usage and outputs are the same as other exec*.py scripts
-Generates execution traces with branch choices indicated for testrun.py


execbp.py

-Should work on any processor with "reset halt" support on openocd
-Generates execution traces by adding breakpoints and debugging from their halt locations
-List of execution traces and length of these traces are read from "./breaks.txt"
-Accepts "0x......" direct memory locations or "sourcefile:linenumber" type addressing for breakpoint locations
-If reset vector breakpoints are required, one can use "break" string instead of memory location addressing
-Outputs are generated in files with name "log[breakpoint_location].txt". 
	e.g. log0x10001048.txt for a breakpoint starting from 0x10001048 memory location
	
casegen.py

-Generates test cases from log.txt file by default, filename can be changed with "-f logfilename" option
-It has two command line options "-l length_of_testcases" and "-n number_of_testcases"
-Resulting test cases are outputted to files with name tcs[0-9]+.txt

testrun.py

-Obtains test cases with name "tcs*.txt" and runs them on HOL, ("tcs" being the default prefix) prefix can be changed with commandline option "-f prefix"
-Expects hol to be in $PATH variable, directly callable from console
-Expects Test.sml HWTest.sml and other necessary HOL testing files to be in the same directory
-It can direct openocd error log to a different file, if commandline option "-o" is used
-Results of tests are outputted to log_tcs*.txt" corresponding to the same respective "tcs*.txt" name included as suffix.
-It also prints number of success and failures after all tests are run in console, after "OVERVIEW" heading.
- [New] It can change the current working directory for test cases with "-d" command line option
-"--nobranch" option prevents it from supplying explicit branch choices to HOL,
	if not used, branches are provided according to the signs in execution traces/test cases
-It skips the test runs for the test cases that previously run, and created "log_" prefixed output files

branchify.py 

- Expects a test case file as an argument. i.e. call with "python branchify.py tcs0.txt" 
- Generates an instruction file with branch choices explicity provided for testrun.py
-The new file is saves with fcs prefix e.g., fcs0.txt
