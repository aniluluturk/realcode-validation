structure Target : Target =
struct

local
open HolKernel Parse boolLib bossLib mips_stepTheory

in

(* l3mips / BERI bluespec boot rom
   We leave space for the register initialisation preamble *)
val preamble_size = (* lo, hi *) 2*7*4 + (* gpr *) 31*6*4 + (* jump *) 4*2
val sram_start = 0x9000000040000000+preamble_size
val sram_size = 2048*4-preamble_size

val init = []

val nextstate = ``NextStateMIPS``
fun pc s = ``^s.PC``
val memory_field_update = ``mips_state_MEM_fupd``
val memory_type = ``:word64 -> word8``
val sort_addresses = updateLib.SORT_WORD_UPDATES_CONV ``:64``
val sort_registers = updateLib.SORT_WORD_UPDATES_CONV ``:5``
val state_type = ``:mips_state``

(* Target specific manipulations to make to step theorems *)
fun fixup_th th =
    th |>
    DISCH_ALL |>
    (* I've seen a few instructions such as
         c049 - stmia   r0!, {r0, r3, r6}
       leave a hypothesis involving LowestSetBit in, despite having
       concrete values everywhere.  I think m0_stepLib is supposed
       to deal with it, but we can do it here instead. *)
(*    REWRITE_RULE [m0Theory.LowestSetBit_def,m0_stepTheory.CountLeadingZeroBits8] |>*)
    UNDISCH_ALL

val word_size = 64
val word_type = ``:word64``
val addr_size = 64

val big_endian = true

(* TODO: fill in / generalise bits of the state as necessary *)

val basic_cp0 =
  ``<| BadVAddr := ARB;
       Cause := ARB;
       Compare := ARB;
       Config := ARB with <| BE := ^(bitstringSyntax.term_of_bool big_endian) |>;
       Count := ARB;
       Debug := ARB;
       EPC := ARB;
       EntryHi := ARB;
       EntryLo0 := ARB;
       EntryLo1 := ARB;
       ErrCtl := ARB;
       ErrorEPC := ARB;
       Index := ARB;
       LLAddr := ARB;
       PRId := ARB;
       PageMask := ARB;
       Random := ARB;
       Status := ARB with <| RE := F; EXL := F; BEV := T |>;
       Wired := ARB;
       XContext := ARB
    |>``

(* TODO: is it possible for us to set up a branch delay slot without executing
   an instruction?  Even if so, would there be any point? *)
val basic_state =
  ``<| BranchDelay := NONE;
       BranchTo := NONE;
       CP0 := ^basic_cp0;
       LLbit := ARB;  (* TODO: should we specify this? *)
       MEM := m;
       PC := pc;
       exceptionSignalled := F;
       exception := NoException;
       gpr := reg;
       (* We always use concrete values for hi and lo; if we don't then Yices
          produces slightly more complex output (e.g., I've just seen it return
          hi = lo, which we don't currently deal with). *)
       hi := SOME hi;
       lo := SOME lo
    |>``;

fun rand_regs gen =
    (* If we were to assign a non-zero value to register 0 it would be ignored
       anyway *)
    let fun rr 0 = ``0w:word64``
          | rr _ = Tools.random_word gen 64 0
        val vals = List.tabulate (32, rr)
        val hol_vals = listSyntax.mk_list (vals, ``:word64``)
    in ``\r:word5. EL (w2n r) ^hol_vals``
    end

fun fill_in_state gen s mem =
    let val regs = rand_regs gen
        val hi = Tools.random_word gen 64 0
        val lo = Tools.random_word gen 64 0
    in subst [{redex = ``reg:word5 -> word64``, residue = regs},
              {redex = ``m:word64 -> word8``, residue = mem},
              {redex = ``hi:word64``, residue = hi},
              {redex = ``lo:word64``, residue = lo}] s
    end

(* No options yet *)
fun describe_additional () = ""
fun additional_constraints (th:Thm.thm) cbv_eval =
  let val (hy,cn) = dest_thm th
      val final_state = rhs cn
      val final_delay = cbv_eval ``^final_state.BranchDelay``
      val final_pc = cbv_eval ``^final_state.PC``
  in [``^final_delay = NONE``, ``(1 >< 0) ^final_pc = 0w : word64``]
  end

val smt_rewrites : Thm.thm list = [mipsTheory.NotWordValue_def]
val smt_simps : Thm.thm list = []

(* Partially apply mips_eval_hex because it does some specialization that
   we don't want to repeat. *)
val step0 = mips_stepLib.mips_eval_hex big_endian
type code = string
fun sc_just [x] = x
  | sc_just _ = failwith "step_code: quote contains multiple strings"
val step_code0 = mips_stepLib.mips_step_code big_endian o sc_just o assemblerLib.quote_to_strings

fun split_branch th =
  case List.find (fn tm => is_eq tm andalso lhs tm = ``s.BranchDelay``) (hyp th) of
      NONE => [th]
    | SOME tm =>
      if optionSyntax.is_some (rhs tm)
      then let val branchdelay = optionSyntax.dest_some (rhs tm)
           in if is_var branchdelay
              then [INST [branchdelay |-> ``NONE : word64 option``] th,
                    INST [branchdelay |-> ``SOME (addr : word64)``] th]
              else [th]
           end
      else [th]

fun split_branches ths = List.concat (map split_branch ths)

val step = split_branches o step0
val step_code = split_branches o step_code0

fun hex s = Option.valOf (StringCvt.scanString (Int.scan StringCvt.HEX) s);

(* The disassembler needs the bytes of 32bit instructions to be split up. *)
fun breakup_hex s =
    let val n = hex s
        val b4 = Int.fmt StringCvt.HEX (n mod 256)
        val b3 = Int.fmt StringCvt.HEX (n div 256 mod 256)
        val b2 = Int.fmt StringCvt.HEX (n div 256 div 256 mod 256)
        val b1 = Int.fmt StringCvt.HEX (n div 256 div 256 div 256)
    in (b1, b2, b3, b4)
    end

fun breakup_hex_llvm_mc s =
    let val (b1,b2,b3,b4) = breakup_hex s
    in "0x" ^ b1 ^ " 0x" ^ b2 ^ " 0x" ^ b3 ^ " 0x" ^ b4 ^ "\n"
    end

fun breakup_hex_printf s =
    let val (b1,b2,b3,b4) = breakup_hex s
    in "\\x" ^ b4 ^ "\\x" ^ b3 ^ "\\x" ^ b2 ^ "\\x" ^ b1
    end

fun print_disassembled_hex s =
    let val i = mips.Decode (Option.valOf (BitsN.fromHexString (s,32)))
    in print (mips.instructionToString i ^ "\n")
    end
(*
    let val _ = OS.Process.system ("scripts/mips-disassemble '" ^ breakup_hex_printf s ^ "'")
    in ()
    end*)
(*    let val llvmmc = Unix.execute ("/bin/sh", ["-c", "llvm-mc -triple=mips64 -disassemble"])
        val (mc_in, mc_out) = Unix.streamsOf llvmmc
        val () = TextIO.output (mc_out, breakup_hex s)
        val () = TextIO.closeOut mc_out
        val s' = TextIO.inputAll mc_in
        val () = TextIO.closeIn mc_in
        val _ = Unix.reap llvmmc
    in print s'
    end
*)

fun encode q =
    let val lines = assemblerLib.quote_to_strings q
        fun line s =
            let val s = mips.stripSpaces s
            in if s = "" then NONE else SOME s
            end
        val instrs = List.mapPartial line lines
    in map (fn s => (mips.encodeInstruction s, 4)) instrs
    end

val split_state_thm = prove(``P s ==>
  (branchdelay = s.BranchDelay) ==>
  (branchto = s.BranchTo) ==>
  (cp0 = s.CP0) ==>
  (llbit = s.LLbit) ==>
  (smem = s.MEM) ==>
  (pc = s.PC) ==>
  (sexception = s.exception) ==>
  (exceptionsignalled = s.exceptionSignalled) ==>
  (sgpr = s.gpr) ==>
  (shi = s.hi) ==>
  (slo = s.lo) ==>
  P (mips_state branchdelay branchto cp0 llbit smem pc sexception
     exceptionsignalled sgpr shi slo)``,
  Cases_on `s` THEN (SIMP_TAC (srw_ss()) []));

(* Due to branch delay slots, we don't choose whether a branch is taken while
   looking at the branch instruction itself, but on the instruction after the
   branch.  We recognise branches by looking to see if the BranchDelay part of
   the state can be filled. *)

datatype isslot = NeverSlot | IsSlot | MaybeSlot (* branch likely instructions *) | MustBranch

fun choose_thms gen thmss =
    let val (_,choices) =
            foldl (fn (thms,(delay_slot,choices)) =>
                      let val rs = map (rand o rhs o concl) thms
                          val delays = map (fn t => Tools.cbv_eval ``^t.BranchDelay``) rs
                          val delay = hd delays
                          val () = if List.all (curry (op =) delay) (tl delays)
                                   then ()
                                   else failwith "Step theorem with variable delay slot"
                          fun must_branch tm =
                              if optionSyntax.is_some tm
                              then optionSyntax.is_some (optionSyntax.dest_some tm)
                              else false
                          fun branch_likely tm =
                              if is_cond tm
                              then let val (_,l,r) = dest_cond tm
                                   in optionSyntax.is_none l orelse optionSyntax.is_none r
                                   end
                              else false
                          val next_delay_slot =
                              if optionSyntax.is_none delay
                              then NeverSlot
                              else if must_branch delay
                              then MustBranch
                              else if branch_likely delay
                              then MaybeSlot
                              else IsSlot
                          val choice = case delay_slot of
                                          NeverSlot => 0
                                        (* Need to allow for a single theorem when it isn't
                                           a branch delay slot *)
                                        | MaybeSlot => if length thms = 1 then 0
                                                       else snd (RandGen.select [0,2] gen)
                                        | IsSlot => 1 + RandGen.genint gen 1
                                        (* This will almost certainly fail if there's
                                           only one theorem, but at least gives it a
                                           chance *)
                                        | Always => if length thms > 2 then 2 else 0
                      in (next_delay_slot,choice::choices)
                      end) (NeverSlot,[]) thmss
    in rev choices
    end

end

end

(*
CombineSteps.combine_steps (map (hd o mips_stepLib.mips_eval_hex false) ["00000000", "21101234", "AF011234", "12345678"]);

*)
