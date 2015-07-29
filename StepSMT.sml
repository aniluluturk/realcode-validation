(* Some attempts to get useful a useful prestate using an SMT solver. *)

structure StepSMT :> StepSMT =
struct

exception Preconditions_impossible

open HolKernel Parse boolLib bossLib State


(* Alternatives for some definitions that aren't supported by the SMT
   translation *)

val word_bit_rw = prove(
  ``!b w:'a word. b <= dimindex (:'a) - 1 ==> (word_bit b w = w ' b)``,
  METIS_TAC [wordsTheory.word_bit_def]);

(* Avoids a round-trip through num, which I think would be awkward for SMT solving *)
val myb2w_def = testingTheory.myb2w_def;

val w2n8_rw = prove(
  ``w2n w = w2n8 w``,
  SRW_TAC [] [YicesSat.w2n8]);

val shift_carry_rw = prove(
  ``!x : word32. testbit 32 (shiftl (w2v x) n) = if (n > 0) /\ (n <= 32) then x ' (32 - n) else F``,
  SRW_TAC [ARITH_ss]
           [bitstringTheory.testbit,bitstringTheory.shiftl_def,bitstringTheory.length_pad_right] THEN
  Q.UNABBREV_TAC `n'` THEN SRW_TAC [ARITH_ss] [listTheory.PAD_RIGHT] THEN
  Cases_on `n <= 32` THEN
  SRW_TAC [ARITH_ss] [rich_listTheory.EL_APPEND1,rich_listTheory.EL_APPEND2,
                      bitstringTheory.length_w2v,wordsTheory.dimindex_32,bitstringTheory.el_w2v]);

val lsl_rw = prove(
  ``!w : 'a word. !n : 'b word. dimindex (:'b) <= dimindex (:'a) ==> (w << w2n n = w <<~ w2w n)``,
  SRW_TAC [] [wordsTheory.word_lsl_bv_def,wordsTheory.w2n_w2w]);

val asr_rw = prove(
  ``!w : 'a word. !n : 'b word. dimindex (:'b) <= dimindex (:'a) ==> ( w >> w2n n = w >>~ w2w n)``,
  SRW_TAC [] [wordsTheory.word_asr_bv_def,wordsTheory.w2n_w2w]);

val asr_n_rw = prove(
  ``!w : 'a word. !n:num. n < dimword (:'a) ==> (w >> n = w >>~ n2w n)``,
  SRW_TAC [] [wordsTheory.word_asr_bv_def,wordsTheory.w2n_w2w]);


val lsr_rw = prove(
  ``!w : 'a word. !n : 'b word. dimindex (:'b) <= dimindex (:'a) ==> (w >>> w2n n = w >>>~ w2w n)``,
  SRW_TAC [] [wordsTheory.word_lsr_bv_def,wordsTheory.w2n_w2w]);

(* We don't currently use this because YicesSat has a definition for #>>. *)
val ror_rw = prove(
  ``!w : 'a word. !n : 'b word. dimindex (:'b) <= dimindex (:'a) ==> (w #>> w2n n = w #>>~ w2w n)``,
  SRW_TAC [] [wordsTheory.word_ror_bv_def,wordsTheory.w2n_w2w]);

local
open wordsTheory

val dimindex_plus_1 = prove(
  ``(FINITE (UNIV:'a -> bool)) ==> (dimindex (:1+'a) = 1 + dimindex (:'a))``,
  (SIMP_TAC arith_ss [fcpTheory.index_sum,dimindex_1,fcpTheory.dimindex_def]) THEN
  (SRW_TAC [] [fcpTheory.dimindex_def]) THEN
  SIMP_TAC arith_ss []);

val dimword_plus_unit = prove(
  ``(FINITE (UNIV:'a -> bool)) ==> (dimword (:unit+'a) = 2 * dimword (:'a))``,
  (SIMP_TAC arith_ss [dimword_def,fcpTheory.index_sum,dimindex_1,fcpTheory.dimindex_def]) THEN
  (SRW_TAC [] [fcpTheory.dimindex_def,arithmeticTheory.EXP_ADD]) THEN
  SIMP_TAC arith_ss []);

in
val add_with_carry_result = prove(
  ``FST (add_with_carry (a,b,c)) = a + b + myb2w c : 'a word``,
(* Cargo culted, perhaps should slim down *)
  SRW_TAC [boolSimps.LET_ss]
    [GSYM word_add_def, add_with_carry_def,
     GSYM word_add_n2w, word_sub_def, WORD_NOT, myb2w_def]);

(* Worse, I've just copied the proof for this! *)
val overflow = prove(
  ``OVERFLOW (a:'a word) b c = (word_msb a  = word_msb b) /\ (word_msb a <> word_msb (a+b+ myb2w c))``,
  SRW_TAC [boolSimps.LET_ss]
    [GSYM word_add_def, add_with_carry_def,
     GSYM word_add_n2w, word_sub_def, WORD_NOT, myb2w_def]);

val carry = prove(
  ``!a b:'a word. !c. FINITE (UNIV:'a -> bool) ==>
    (CARRY_OUT a b c =
       word_msb ((word_join (0w:1 word) a) + (word_join (0w:1 word) b) + myb2w c))``,
  Cases THEN Cases THEN STRIP_TAC THEN
  (SIMP_TAC (arith_ss++boolSimps.LET_ss) [add_with_carry_def]) THEN
  (SRW_TAC [] [myb2w_def,word_join_0,word_add_n2w,word_msb_n2w_numeric,w2w_def,
               INT_MIN_def,dimindex_plus_1,dimword_plus_unit]) THENL
  [(`n + n' + 1 < 2 * dimword (:'a)` by DECIDE_TAC),
   (`n + n' < 2 * dimword (:'a)` by DECIDE_TAC)
  ] THEN
  (SRW_TAC [] [dimword_def]) THEN
  (SIMP_TAC arith_ss []));

val carry32 = prove(
  ``!a b:word32. !c.
    (CARRY_OUT a b c =
       word_msb ((word_join (0w:1 word) a) + (word_join (0w:1 word) b) + myb2w c))``,
  SRW_TAC [] [carry]);

end

fun early_rewrites th =
    SIMP_RULE (srw_ss()) [word_bit_rw,add_with_carry_result,overflow,carry,
                          shift_carry_rw,
                          lsl_rw, lsr_rw, asr_rw, (*ror_rw,*)
                          w2n8_rw] th

(* We don't want to undo the rewrites during reductions that happen before we
   go to the SMT solver. *)

local open wordsSyntax in
val non_eval_terms = [word_lsl_bv_tm,
                      word_lsr_bv_tm,
                      word_asr_bv_tm,
                      word_ror_tm,
                      word_div_tm,
                      word_sdiv_tm,
                      word_mod_tm,
                      word_srem_tm]
end

val cbv_CONV = Tools.mk_cbv_CONV non_eval_terms
val cbv_eval = Tools.conv_term cbv_CONV


(* Flags for additional constraints *)
val no_self_modification_flag = ref true
fun desc_bool true = "on"
  | desc_bool false = "off"
fun describe_additional () =
    "no_self_modification:" ^ desc_bool (!no_self_modification_flag) ^
    "," ^ Target.describe_additional ()


(* See also stateLib.write_footprint; I'm not entirely happy with that exception
   handler.  The first couple of nested functions go through any auxiliary
   mem_step_n definitions introduced by CombineSteps to prevent blow-up due
   to aliasing. *)
fun extract_writes hyps tm =
    let fun find_hyp m =
            case List.find (fn tm =>
                               is_comb tm andalso 
                               rator tm = ``Abbrev`` andalso
                               is_eq (rand tm) andalso
                               lhs (rand tm) = m) hyps of
                NONE => failwith ("Unable to find memory variable " ^ term_to_string m ^ " in hypotheses")
              | SOME tm => rhs (rand tm)
        fun extract tm =
            let val (upd,m) = combinSyntax.strip_update tm
            in if is_var m then
                   if dest_var m = ("m", Target.memory_type)
                   then upd
                   else upd@(extract (find_hyp m))
               else failwith ("Expected memory variable \"m\", got " ^ term_to_string m)
            end
    in
        (case strip_comb tm of
             (tm, [v, rst]) =>
             if tm = Target.memory_field_update
             then (extract (combinSyntax.dest_K_1 v))@(extract_writes hyps rst)
             else extract_writes hyps rst
           | _ => [])
    end handle HOL_ERR {message = "not a const", ...} => []
fun write_footprint hyps x = map fst (extract_writes hyps x)


(* To handle some things properly, in particular to provide a simple translation
   of the Aligned predicate suitable for feeding to the solver, it's easier to
   generate a new term based on informal examination of the precondition.  We
   will end up checking that the values we get from SMT solving are good anyway,
   so we don't need a formal link here. *)

local
    val low_mem = wordsSyntax.mk_wordii (Target.sram_start,Target.word_size)
    val high_mem = ``^(low_mem) + ^(wordsSyntax.mk_wordii (Target.sram_size,Target.word_size))``
    fun check_mem addr = ``(^addr >= ^(low_mem)) /\ (^addr < ^(high_mem))``
    fun check_mem_access i = check_mem ``(memory_address : num -> ^(ty_antiq Target.word_type)) ^(numSyntax.term_of_int i)``

    fun record_write i addr = ``write_address ^(numSyntax.term_of_int i) = ^addr``
    fun check_write_address i = check_mem ``(write_address : num -> ^(ty_antiq Target.word_type)) ^(numSyntax.term_of_int i)``

    fun mapi f l =
        let fun aux _ [] = []
              | aux n (h::t) = f n h :: aux (n+1) t
        in aux 0 l
        end
in

val record_writes = mapi record_write

fun no_self_modification instr_lengths write_num =
    let val write = ``(write_address : num -> ^(ty_antiq Target.word_type)) ^(numSyntax.term_of_int write_num)``
        fun aux i len =
            let val i = numSyntax.term_of_int i
                val len = wordsSyntax.mk_wordi (Arbnum.fromInt len,Target.word_size)
            in ``(^write < instr_start ^i) \/ (^write >= instr_start ^i + ^len)``
            end
    in mapi aux instr_lengths
    end

(* NB: assumes straight-line code, no jumps *)
fun add_harness n poststate h =
    let val instrs = Harness.harness_instrs h
        val post_pc = cbv_eval (Target.pc poststate)
        fun add_byte (byte, off) =
            let val off = wordsSyntax.mk_wordii (off, Target.addr_size)
                val byte = wordsSyntax.mk_wordii (byte, 8)
            in [``(m (^post_pc + ^off) = ^byte)``,
                check_mem ``^post_pc + ^off``]
            end
        fun add_bytes (bytes,off) =
           foldl (fn (b,(i,cs)) => (i+1,(add_byte (b,i))@cs)) (off,[]) bytes
        fun add (ins,(i,off,constrs)) =
            let val i' = numSyntax.term_of_int i
                val (off', constrs') = add_bytes (ins, off)
                val off = wordsSyntax.mk_wordii (off, Target.addr_size)
            in (i+1, off',
                ``(instr_start ^i' = ^post_pc + ^off)``::
                 constrs'@constrs)
            end
        val (_,_,constrs) = foldl add (n,0,[]) instrs
    (* Return the lengths of the new instructions, plus constraints *)
    in (map length instrs, constrs)
    end

fun trans_simple_thm th instr_lengths mem_counter harness =
    let val (hy,cn) = dest_thm th
        val (_,rhs) = dest_eq cn
        val write_footprint = write_footprint hy rhs
        val writes = length write_footprint
        val write_recording = record_writes write_footprint
        val write_checks = List.tabulate (writes, check_write_address)
        val (harness_lengths, harness_conds) = add_harness (length instr_lengths) rhs harness
        val no_self_mod =
            if !no_self_modification_flag
            then List.concat (List.tabulate (writes,
                     (no_self_modification (instr_lengths@harness_lengths))))
            else []
        val read_checks = List.tabulate (mem_counter, check_mem_access)
        val target_checks = Target.additional_constraints th cbv_eval
    in (writes, harness_conds @
                no_self_mod @
                write_recording @ write_checks @
                read_checks @ 
                target_checks @ 
                hy)
    end
    handle e => raise wrap_exn "StepSMT" "trans_simple_thm" e
end

fun simplify_thm th =
    let val th = INST [{redex = mk_var ("s", Target.state_type),
                        residue = Target.basic_state}] th
        (* Simplify away bit vectors (into words), Aligned and trivial propositions *)
        val th = th |>
                    DISCH_ALL |>
                    REWRITE_RULE Target.smt_rewrites |>
                    CONV_RULE (DEPTH_CONV bitstringLib.v2w_n2w_CONV) |>
                    (* Most of the rewrites from the list of theorems now happen
                       in early_rewrites, but are still kept here in case one is
                       exposed between there and here.  With a little care we
                       could slim this list down (probably to word_bit_rw,
                       w2n8_rw and myb2w_def). *)
                    SIMP_RULE (srw_ss()) ([word_bit_rw,add_with_carry_result,overflow,carry,
                                           shift_carry_rw,
                                           lsl_rw, lsr_rw, asr_rw, (*ror_rw,*)
                                           w2n8_rw,myb2w_def] @ Target.smt_simps)
    in th
    end

fun cleanup_results simp comp =
  comp (simp (arith_ss++wordsLib.WORD_ss) [],
       (* Put shift by a constant back into bit vector form, and deal with any
          remaining uses of word_bit *)
        simp arith_ss [asr_n_rw, wordsTheory.dimword_32, wordsTheory.dimword_64,
                       wordsTheory.word_bit_def])

fun trans_thm th instr_lengths mem_counter harness constraints =
    (* Instantiate the state to remove irrelevant stuff and name components
       individually *)
    let val th = simplify_thm th |>
                 UNDISCH_ALL
        val () = if concl th = ``T`` then raise Preconditions_impossible else ()
        (* Get the preconditions *)
        val (write_counter, hol_preconds) = 
            trans_simple_thm th instr_lengths mem_counter harness
        val hol_preconds = hol_preconds @ constraints
        (* Make a single term *)
        val hol_precond = List.foldl mk_conj ``T`` hol_preconds
        val (hy,cc) = (hd o fst) (cleanup_results SIMP_TAC (op THEN) ([],hol_precond))
        val preamble_conds = Harness.preamble_constraints ()
    in (hy, List.foldl mk_conj cc preamble_conds)
    end
    handle e as HOL_ERR _ => raise wrap_exn "StepSMT" "trans_thm" e

fun trans_thm_for_test th =
    th |> simplify_thm |> cleanup_results SIMP_RULE (fn (f,g) => g o f)




fun augment_state s assignments =
    let val subs = map (fn (x,y) => {redex = x, residue = y}) assignments
    in subst subs s
    end

val map_type = ``:num -> ^(Target.word_type)``

fun get_map name assignments =
    let val var = List.find (equal name o fst) assignments
        val arb = ``ARB : ^(ty_antiq map_type)``
    in case var of
         SOME (_,v) => subst [{redex = name, residue = arb}] v
       | NONE => arb
    end

val instr_starts = get_map (mk_var ("instr_start", map_type))
val memory_addresses = get_map (mk_var ("memory_address", map_type))
val write_addresses = get_map (mk_var ("write_address", map_type))

end

(* So that we can see the variable name choices and temporary files for
   debugging purposes.
Feedback.set_trace "HolSmtLib" 4;
*)
