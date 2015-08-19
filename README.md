# realcode-validation #

This repository contains the source files for real code testing project for ARM Cortex-M0. 

-target/Target.sml is currently configured for XMC board, configurations for STM and NXP boards can be done with commenting out the related sections
-after Target changes are made, for generating initialising the environment, call "holmake" in the directory
-finally, you can run HOL4 by running "hol" in the current directory, or you can use automated scripts for testing

-------

.py files are automated testing scripts and,

.sml files are HOL scripts are adopted from https://bitbucket.org/bacam/m0-validation

/tests directory contain the previous test cases generated, and can be used freely with the testrun.py

----------- python scripts ----------

exec1.py - allows simple executables to be traced from reset vector

exec2.py - does the same task with exec1.py, but for STM board (or any regular resettable board)

exec3.py - provides branch choice logging during tracing 

execbp.py - allows breakpoints based tracing, instead of reset vector tracing

casegen.py - creates test cases from execution traces

testrun.py - constructs test case commands and executes them against HOL interactive session

