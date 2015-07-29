open Test

(* Make displaying states and theorems that contain 8kB of memory bearable. *)
val () = max_print_depth := 200;

(* Example to generate a test case (without the full memory), but don't run it *)
val (_, ex_instrs, ex_prestate, ex_th, ex_instr_start, ex_memory_addresses, ex_write_footprint) =
  construct_test NONE (Generate 5) NONE (Basic Breakpoint) [];

val (_, ex_instrs, ex_prestate, ex_th, ex_instr_start, ex_memory_addresses, ex_write_footprint) =
  construct_test_code `str r0, [r1]` NONE (Basic Breakpoint) [];

(* Now for the real testing against the device.  For further
   experimentation without a device, look for the
   "cut and paste" debugging stuff further down. *)

(* Manual ----- *)
val debug = HWTest.connect();


run_test_code debug
  `LDR	r0, [pc, #0x8]	; 0x10001024
   MOV	r13, r0
   LDR	r0, [pc, #0x8]	; 0x10001028
   BLX	r0` NONE (Basic Breakpoint) [];


run_test_code debug
  `#0x4802
   4685
   4802
   4780` NONE (Basic Breakpoint) [];


Example from STP talk

run_test debug NONE (Manual [
("4802", "", 2),
("4685", "", 2),
("4802", "", 2),
("4780", "", 2)
]) NONE (Basic Breakpoint) [];



run_test_code debug
  `NOP			; (MOV r8, r8)
   BX	r14
   B	0x10001048
   LDR	r0, [pc, #0x44]	; 0x10001090` NONE (Basic Breakpoint) [];

run_test_code debug
  `B -#0x10001048
   LDR	r0, [pc, #0x44]	; 0x10001090` NONE (Basic Breakpoint) [];

run_test_code debug
  `NOP			; (MOV r8, r8)
   BX	r14
   LDR	r0, [pc, #0x8]	; 0x10001028
   LDR	r0, [pc, #0x44]	; 0x10001090` NONE (Basic Breakpoint) [];

run_test debug NONE (Generate 5) NONE (Basic Breakpoint) [];
run_test debug (SOME (!last_random_trace)) (Generate 5) NONE (Basic Breakpoint) [];
(* run_test debug (SOME (Tools.read_file "/tmp/m0log/random/10")) (Generate 5) NONE (Basic Breakpoint) []; *)
run_test_code debug
  `ldrsh   r0, [r1, r2]
   lsls    r0, r2
   adds    r0, r0, r2
   ldr     r3, [r0, #0]` NONE (Basic Breakpoint) [];

(* Example I'm thinking of using in a write-up *)
run_test_code debug
  `ldrsh   r0, [r1, r2]
   lsls    r0, r0, r2
   bcs     +#12
   add     r0, r0, r2
   ldr     r3, [r0, #0]` (SOME [0,0,0,0,0]) (Basic Breakpoint) [];

(* Loop-the-loop; you can check that it's looping by looking at the instruction
   locations that are reported.  You can also turn on SMT feedback to see the
   corresponding constraints. *)
run_test_code debug
  `adds    r0, r0, r2
   bcs     -#2
   adds    r0, r0, r2
   bcs     -#2`
  (SOME [0, 0 (* take first time *), 0, 1 (* pass second time *)])
  (Basic Breakpoint) [];

run_test_code debug
  `adds    r0, r0, r2
   bcs     -#2
   adds    r0, r0, r2
   bcs     -#2
   adds    r0, r0, r2
   bcs     -#2
   adds    r0, r0, r2
   bcs     -#2
   adds    r0, r0, r2
   bcs     -#2`
  (SOME [0, 0 (* take first time *), 0, 0, 0, 0, 0, 0, 0, 1 (* pass last time *)])
  (Basic Breakpoint) [];

(* Explore behaviour near the end of memory (on STM32F0) *)

run_test_code debug `add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20001ffew : word32``];
run_test_code debug `add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20001ffcw : word32``];
run_test_code debug `add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20001ffaw : word32``];

run_test_code debug `b -#12` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20001ffew : word32``];

(* Version for the Cyprus PSoC, with penalty stores *)
run_test_code debug
  `str r0, [r1]
   add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20000ffaw : word32``];

run_test_code debug
  `str r0, [r1]
   nop
   add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20000ffaw : word32``];

run_test_code debug
  `str r0, [r1]
   nop
   nop
   nop
   add pc, r3` NONE (Basic Breakpoint)
  [``instr_start 2 = 0x20000ffaw : word32``];

run_test_code debug
  `str r0, [r1]
   bl -#256` NONE (Basic Breakpoint)
  [``instr_start 0 = 0x20000ffaw : word32``];


(* Show that looping at the end of memory causes a multi-cycle anomaly *)
run_test_code debug
  `adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12`
  (SOME [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0])
  (Basic Breakpoint) [``instr_start 2 = 0x20001ffew : word32``];

(* Shorter version for paper *)
run_test_code debug
  `adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12
   b       -#4
   adds    r0, r0, r2
   bcs     -#12`
  (SOME [0, 1, 0, 0, 1, 0, 0, 0])
  (Basic Breakpoint) [``instr_start 2 = 0x20001ffew : word32``];

HWTest.close debug;

(* Batch ----- *)

val debug = HWTest.connect();

logged_run "/tmp/m0log" debug 5 (Basic Breakpoint) 1000 [];

(* An example of extra constraints: here we force an instruction into the last
   word of the STM32F0's SRAM *)
logged_run "/tmp/m0log-10-high" debug 10 (Basic Breakpoint) 5 [``instr_start (some_instr:num) = 0x20001ffew : word32``,``some_instr < 10``];

(* Here we forbid it.  I've not been able to get Yices (or maybe the
   translation?) to cope with a universal quantifier for n). *)
logged_run "/tmp/m0log-5-not-high" debug 5 (Basic Breakpoint) 100 
  (List.tabulate (5, fn n => ``instr_start ^(numSyntax.term_of_int n) < 0x20001ffcw : word32``));



(* A little code to help cut and paste debugging; the idea is to obtain the
   arguments to run_test so that you can process each step manually. *)

fun to5tup instrs choices harness extra = (NONE : string option, Manual instrs, SOME choices, harness, extra : term list)

(* Example
val (gen,instrs,choices,harness,constraints) = to5tup
  [("5896", "LDR (reg)", 2), ("4810", "LDR (lit)", 2), ("77C7", "STRB (imm)", 2), ("D7E4", "B T1", 2), ("B0D1", "SUB (SP-imm)", 2)]   [0,0,0,0,0]   (NOP_padding_after (3,Breakpoint)) [];

Then you can step through bits of Test.construct_test and Test.run_test; the 
variables already have the correct names.
*)

(* Similar, but starting with assembly. *)

fun codetup code (choices : int list option) harness extra =
    let val hex = m0AssemblerLib.m0_code code
        val instrs = map (fn h => (h, "", if String.size h > 4 then 4 else 2)) hex
    in (NONE : string option, Manual instrs, choices, harness,extra:term list)
    end

(* Example
val (gen,instrs,choices,harness,constraints) = codetup
  `ldrsh   r0, [r1, r2]
   lsls    r0, r2
   adds    r0, r0, r2
   ldr     r3, [r0, #0]` NONE (Basic Breakpoint) [];
*)



(* Previously problematic instruction sequence to test with:

Shifts not dealt with properly, but tends to run anyway; now works as intended:
run_test debug NONE (Manual [("4137", "ASR (reg)", 2), ("D333", "B T1", 2)]) NONE (Basic Breakpoint) [];

Depends on flag from ADD, now works:
run_test debug NONE (Manual [("1856", "ADD (reg) T1", 2), ("D74F", "B T1", 2), ("D5A9", "B T1", 2)]) NONE (Basic Breakpoint) [];

Can't remember what was wrong, now works (!):
run_test debug NONE (Manual [("42A0", "CMP (reg) T1", 2), ("4388", "BIC (reg)", 2), ("D982", "B T1", 2)]) NONE (Basic Breakpoint) [];

Runs away, due to incomplete memory checks, fixed now:
run_test debug NONE (Manual [("57A8", "LDRSB (reg)", 2), ("42FC", "CMN (reg)", 2), ("46C5", "MOV (reg) T1", 2), ("1CF0", "ADD (imm) T1", 2), ("D286", "B T1", 2)]) NONE (Basic Breakpoint) [];

"Pre-state did not satisfy all hypotheses:\nmode \226\137\160 Mode_Handler", now fixed
run_test debug NONE (Manual [("43E3", "MVN", 2), ("9AE4", "LDR (imm) T2", 2), ("DA8B", "B T1", 2), ("D559", "B T1", 2), ("4710", "BX", 2)]) NONE (Basic Breakpoint) [];

If the wrong branch is chosen, there's no pre-state because the EOR is always zero.
run_test debug NONE (Manual [("28FE", "CMP (imm)", 2), ("1B4E", "SUB (reg)", 2), ("4064", "EOR (reg)", 2), ("AF29", "ADC (SP+imm) T1", 2), ("D02B", "B T1", 2)]) (SOME [0,0,0,0,0]) (Basic Breakpoint) []; (* works *)
run_test debug NONE (Manual [("28FE", "CMP (imm)", 2), ("1B4E", "SUB (reg)", 2), ("4064", "EOR (reg)", 2), ("AF29", "ADC (SP+imm) T1", 2), ("D02B", "B T1", 2)]) (SOME [0,0,0,0,1]) (Basic Breakpoint) []; (* impossible *)

Fails to translate to Yices because EVAL_RULE in ComposeSteps is unfolding something, fixed.
run_test debug NONE (Manual [("14E8", "ASR (imm)", 2), ("3C4E", "SUB (imm) T2", 2), ("4088", "LSL (reg)", 2), ("2206", "MOV (imm)", 2), ("D997", "B T1", 2)]) NONE (Basic Breakpoint) [];

Exception raised at StepSMT.trans_thm:\nat StepSMT.trans_simple_thm:\nat boolSyntax.dest_eq(_ty):\nnot an \"=\"\n
Found to be vacuous in StepSMT, rather than in the combine function above, now fixed
run_test debug NONE (Manual [("4730", "BX", 2), ("46B7", "MOV (reg) T1 Rd=PC", 2), ("408C", "LSL (reg)", 2), ("D", "MOV (reg) T2", 2), ("1A85", "SUB (reg)", 2)]) NONE (Basic Breakpoint) [];

Yices.translate_term: uninterpreted constant LowestSetBit, fixed now
run_test debug NONE (Manual [("B563", "PUSH", 2), ("30E3", "ADC (reg) T2", 2), ("4259", "RSB (imm)", 2), ("DB0F", "B T1", 2), ("C049", "STM, STMIA, STMEA", 2)]) NONE (Basic Breakpoint) [];

Failed register comparison: RName_SP_main; bad stack pointer restriction, now fixed
run_test debug NONE (Manual [("B213", "SXTH", 2), ("6C21", "LDR (imm) T1", 2), ("31B0", "ADC (reg) T2", 2), ("40A8", "LSL (reg)", 2), ("448D", "ADD (reg) T2, ADD (SP+reg)", 2)]) NONE (Basic Breakpoint) [];

Yices.translate_term: uninterpreted constant EL (and lots more); overzealous reduction, now fixed
run_test debug NONE (Manual [("B248", "SXTB", 2), ("6B91", " LDR (imm) T1", 2), ("215D", " MOV (imm)", 2), ("4091", " LSL (reg)", 2), ("DDF6", " B T1", 2)]) NONE (Basic Breakpoint) [];

Yices.translate_term: uninterpreted constant $, w2n; same, now fixed
run_test debug NONE (Manual [("773D", "STRB (imm)", 2), ("413B", " ASR (reg)", 2), ("1699", " ASR (imm)", 2), ("D4BB", " B T1", 2), ("D16F", " B T1", 2)]) NONE (Basic Breakpoint) [];

Yices.translate_term: uninterpreted constant w2n; more overzealous reduction, now fixed
(NB: most combinations of branches are unsatisfiable, but there is a useful one.)
run_test debug NONE (Manual [("40B4", "LSL (reg)", 2), ("46A7", "MOV (reg) T1 Rd=PC", 2), ("D431", " B T1", 2), ("D3C0", " B T1", 2), ("DDFF", " B T1", 2)]) (SOME [0,0,1,0,0]) (Basic Breakpoint) [];

Used to blow up due to memory aliasing checks
run_test debug NONE (Manual [("5987", "LDR (reg)", 2), ("B4A2", "PUSH", 2),
    ("C9EF", "LDM, LDMIA, LDMFD", 2), ("4552", "CMP (reg) T2", 2),
    ("5806", "LDR (reg)", 2)]) NONE (Basic Breakpoint) [];

Cycle count mismatches.  Retrospectively, these are instruction sequences where
Yices happens to pick the final word for one of the instructions.

run_test debug NONE (Manual [("1251", "ASR (imm)", 2), ("D648", "B T1", 2), ("46F7", "MOV (reg) T1 Rd=PC", 2), ("AD44", "ADC (SP+imm) T1", 2), ("44E7", "ADD (reg) T2 Rdn=PC", 2)]) NONE (Basic Breakpoint) [];

run_test debug NONE (Manual [("BA68", "REV16", 2), ("D4CC", "B T1", 2), ("E770", "B T2", 2), ("460C", "MOV (reg) T1", 2), ("8F16", "LDRH (imm)", 2)]) (SOME [0,0,0,0,0]) (Basic Breakpoint) []; OK
run_test debug NONE (Manual [("BA68", "REV16", 2), ("D4CC", "B T1", 2), ("E770", "B T2", 2), ("460C", "MOV (reg) T1", 2), ("8F16", "LDRH (imm)", 2)]) (SOME [0,1,0,0,0]) (Basic Breakpoint) []; Fail, first branch not taken
Note that the second branch is unconditional

run_test debug NONE (Manual [("B22A", "SXTH", 2), ("D895", "B T1", 2), ("BF00", "NOP", 2), ("437F", "MUL", 2), ("936B", "STR (imm) T2", 2)]) (SOME [0,0,0,0,0]) (Basic Breakpoint) [];  Fails, branch taken
run_test debug NONE (Manual [("B22A", "SXTH", 2), ("D895", "B T1", 2), ("BF00", "NOP", 2), ("437F", "MUL", 2), ("936B", "STR (imm) T2", 2)]) (SOME [0,1,0,0,0]) (Basic Breakpoint) [];  OK

run_test debug NONE (Manual [("2137", "MOV (imm)", 2), ("B255", "SXTB", 2), ("D92B", "B T1", 2), ("41E1", "ROR (reg)", 2), ("DA28", "B T1", 2)]) (SOME [0,0,0,0,1]) (Basic Breakpoint) [];



Example from STP talk

run_test debug NONE (Manual [
("5e88", "ldrsh   r0, [r1, r2]", 2),
("4090", "lsls    r0, r2", 2),
("1880", "adds    r0, r0, r2", 2),
("6803", "ldr     r3, [r0, #0]", 2)
]) NONE (Basic Breakpoint) [];


Anthony realised that there was an ARMv7 instruction that the M0 doesn't
support left in the model.  I could test that it doesn't work with

run_test debug NONE (Manual [("f0008001", "bad", 4)]) NONE (Basic Breakpoint) [];

(it now fails in the model, too; previously only running it on the
device revealed the problem) and check a related, but supported, BL
instruction with

run_test debug NONE (Manual [("f000f801", "good", 4)]) NONE (Basic Breakpoint) [];

*)



(* Test cases for investigating the LPC11u14's curious timing
behaviour on store instructions located in odd half-words.

run_test_code debug
 `nop
  nop
  nop
  nop
  str r1, [r0, #0]
  nop
  nop
  nop
  str r1, [r0, #0]
  nop
  nop
  nop
  nop`
 NONE (Basic Breakpoint)
 [``instr_start 0 = 0x10000002w : word32``,
  ``(reg : RName -> word32) RName_0 = 0x10000100w : word32``];

run_test_code debug
 `stm r0!, {r1-r7}`
 NONE (Basic Breakpoint)
 [``instr_start 0 = 0x10000002w : word32``,
  ``(reg : RName -> word32) RName_0 = 0x10000100w : word32``];

run_test_code debug
 `str r1, [r0, r2]
  subs r2, r2, #4
  bne -#4
  str r1, [r0, r2]
  subs r2, r2, #4
  bne -#4
  str r1, [r0, r2]
  subs r2, r2, #4
  bne -#4`
 (SOME [0,0,0,0,0,0,0,0,1]) (Basic Breakpoint)
 [``instr_start 0 = 0x10000002w : word32``];

run_test_code debug
 `nop
  nop
  nop
  ldr r1, [r0, #0]`
 NONE (Basic Breakpoint) [``instr_start 0 = 0x10000000w : word32``];

*)
