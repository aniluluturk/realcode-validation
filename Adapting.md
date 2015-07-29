# Adapting the testing to other targets

Each test goes through several stages:

1. Instruction sequence generation
2. Combining step theorems
3. Translating preconditions into SMT format, while adding extra constraints
4. Filling in the randomised background state
5. Instantiating the theorem with the prestate
6. Running the test case and comparing the poststate

Throughout the process we would need to replace or generalise terms and types
which refer to the M0 model, such as the width of a word, register file, state
field names, etc.

The target-specific code is found in the `target-`name directory.  To
switch target, change the `target` symlink to point to the relevant
directory, and edit `TARGET` in `Holmakefile`.

### Instruction sequence generation

The current generator uses a list of instruction formats for the
Cortex-M0, and even the datatype for the formats is specialised.  For
another target there are several options:

* Use the M0 generator as a template.
* QuickCheck-like generation of the instruction datatype, using an encoding
  function generated from the L3 model (if available).
* A generic version of the latter, using HOL's `TypeBase` to extract the
  structure of the instructions.

A similar instruction generator has been constructed for MIPS, with
the list of instructions taken from the decoder.

The top-level choice of which step theorems to use for each
instruction is now implemented in `Target`, because the MIPS model
requires you to make the choice at the branch delay slot, not the
branch itself.

### Combining step theorems

A number of target specific terms and types are used from `Target`,
plus a conversion to sort register updates and a function to perform
any minor changes to the step theorems that we want to do immediately.

### SMT translation

The set of rewrites to carry out, details of optional constraints,
memory location and size and the usual types and terms are retrieved
from `Target`.  Instructions for the harness come from `Harness`, and
are assumed to be straight line code.  Note that `Target` may need to
provide constraints to ensure the harness can run correctly (e.g.,
that it is properly aligned).

Note that the `YicesTest` file has been updated so that a systematic
trial of the Yices conversion can be done for a sample of instructions
from new targets.

### Filling in the background state

There is now an option controlling whether the full memory area is
filled in, and if so where it is held.  The variable
`Prestate.full_memory_option` controls this, with `FullHOL` filling in
the memory in the HOL state, `FullSML` keeping it separate as an SML
value, and `Partial` only filling in the values specified by the SMT
solver, i.e, the minimal footprint predicted by the model.

### Instantiating the theorem

This required a little more generalisation than expected, because the
Yices solver output varied a little more than expected.

### Running the test and comparing

This is implemented per-target in `HWTest`.

## Potential MIPS problem areas

* Exceptions (e.g., fault on arithmetic overflow) are supported by the model,
  so we might have to think about how testable these are, and whether to avoid
  or to provoke them (or to leave the choice to the SMT solver).  For example,
  if we were testing inside a process on a machine running an operating system
  it obviously wouldn't make sense to allow exceptions because we can't
  replace the handler.  *Status*: Not considered yet
