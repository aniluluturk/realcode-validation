# Randomised test case generation for the MIPS model

1. Change the `target` symlink to point to `target-MIPS`
2. Change the definition of `TARGET` in `Holmakefile` to `MIPS`
3. Run `Holmake`
4. Ensure `l3mips` is in your `PATH` (if you want it to automatically
   compare the HOL prediction with it)
5. Run HOL, and

    load "Test";
    open Test;
    (* It doesn't connect to real hardware for MIPS, so it just returns unit *)
    val debug = HWTest.connect ();
    (* This isn't necessary, but is faster *)
    Prestate.full_memory_option := Prestate.FullSML;

Then you can generate a single test (which is written to `/tmp/test.mem`) with

    run_test debug NONE (Generate 10) NONE (Basic Breakpoint) [];

where 10 is the number of instructions that will be in the sequence. A
batch of tests can be generated with

    logged_run "/tmp/mips-tests" debug 10 (Basic Breakpoint) 1000 [];

which will try to generate tests for 1000 sequences of 10 instructions
each.  Successfully generated tests are place in the `/tmp/mips-test`
directory.  Note that many individual attempts to generate a test will
fail (for example, because the instructions would require a register
to contain a 64-bit address, but also a 32-bit value, which are
incompatible).

After successfully generating a test the code will attempt to compare
the HOL prediction of the registers with the output from `l3mips`.  An
`OK` or `Failed` will appear as appropriate.  If `l3mips` isn't in
your `PATH`, then it will report `Failed` all the time.  The tests are
generated regardless.
