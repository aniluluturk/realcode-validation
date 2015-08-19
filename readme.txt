This document outlines the usage and I/O specifications of scripts provided within the project for automated testing/validation process


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
-List of execution traces and length of these traces are read from "./breakpoints.txt"
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
