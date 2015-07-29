# realcode-validation

This repository contains the source files for real code testing project for ARM Cortex-M0. 

.py files are automated testing scripts and,

.sml files are HOL scripts are adopted from https://bitbucket.org/bacam/m0-validation

/tests directory contain the previous test cases generated, and can be used freely with the testrun.py

----------- python scripts ----------

exec1.py - allows simple executables to be traced from reset vector

execbp.py - allows breakpoints based tracing, instead of reset vector tracing

casegen.py - creates test cases from execution traces

testrun.py - constructs test case commands and executes them against HOL interactive session

