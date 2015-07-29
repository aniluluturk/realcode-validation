structure CombineSteps :> CombineSteps =
struct

open HolKernel Parse boolLib bossLib

(* Definition for executing n steps.
   Note that we don't want the definition to be unfolded by computeLib, so use
   zDefine. *)

val NStates_def = testingTheory.NStates_def

val after_step = GSYM (CONJUNCT2 NStates_def);

val one_step =
    Tactical.prove (``(^(Target.nextstate) s = SOME s') ==> (NStates 1 s = s')``,
                    STRIP_TAC THEN
                    (ASM_REWRITE_TAC [numLib.num_CONV ``1:num``, NStates_def]) THEN
                    (SIMP_TAC std_ss []));

fun note_mem th counter =
    let fun is_mem_access tm =
            if is_comb tm
            then ((equal ``s.MEM``) o rator) tm
            else false
        val tms = (concl th)::(hyp th)
        val accesses = List.concat (map (find_terms is_mem_access) tms)
    in List.foldl (fn (access,(th,counter)) =>
                      (ADD_ASSUM ``memory_address ^(numSyntax.term_of_int counter) = ^(rand access)`` th,
                       counter+1))
                  (th,counter) accesses
    end

(* Functions for combining step theorems. *)

val s = mk_var ("s", Target.state_type)
val pc_s = Target.pc s

(* Replace lefthand-side of result with NStates 1 s *)
fun last_step n th = 
    let val (th, mem_counter) = note_mem th 0
    in (th |>
           MATCH_MP one_step |>
           ADD_ASSUM ``instr_start ^(numSyntax.term_of_int n) = ^pc_s``,
        mem_counter)
    end
    handle e => raise wrap_exn "CombineSteps" "last_step" e

(* The only common free variable we want between the step theorems is the
   state, s.  Every other variable should be distinct or we'll overconstrain
   the pre-state. *)
fun freshen_free th th' =
    let val avoid = Thm.thm_frees th'
        val have = Thm.thm_frees th
        fun change v =
            if v = s then NONE else
            SOME {redex = v, residue = Term.prim_variant avoid v}
        val subs = List.mapPartial change have
    in INST subs th
    end

(* Memory updates can lead to a blow-up in the theorem size, so separate each
   one. *)
fun extract_update thm =
  rand (rand (find_term (fn t => (rator t = Target.memory_field_update) handle _ => false) (concl thm)));

fun abstract_memory_update n thm =
  let val v = extract_update thm
      val var = mk_var ("mem_step_" ^ int_to_string n, Target.memory_type)
      val eq_thm =
        ISPECL [var,v] EQ_SYM |>
        CONV_RULE (LAND_CONV (ONCE_REWRITE_CONV [GSYM markerTheory.Abbrev_def])) |>
        UNDISCH 
  in PURE_REWRITE_RULE [eq_thm] thm
  end handle HOL_ERR _ => thm


fun add_prev_step n new_th (tail_th, mem_counter) =
    (* Note that there is an earlier step *)
    let val new_th = freshen_free new_th tail_th
        val (new_th,mem_counter) = note_mem new_th mem_counter
        val new_th = abstract_memory_update n new_th
        val th = INST [{redex = mk_var ("s", Target.state_type),
                        residue = ``THE (^(Target.nextstate) s)``}] tail_th
        val th = REWRITE_RULE [after_step] th
        (* Substitute the previous step theorem *)
        val th = th |>
                 DISCH_ALL |>
                 SUBST_MATCH new_th |>
                 DISCH_ALL |>
                 (* Simplify things down; use EVAL_RULE rather than Tools.m0_CONV
                    because we don't want to reveal internal details here (in
                    particular, the definition of Aligned) *)
                 computeLib.RESTR_EVAL_RULE StepSMT.non_eval_terms |>
                 SIMP_RULE (arith_ss++wordsLib.WORD_ss) [] |>
                 (* Get rid of redundant updates and sort to make updates more readable *)
                 (* abstract_memory_update makes sorting the memory field pointless
                 CONV_RULE (ONCE_DEPTH_CONV Target.sort_addresses) |> *)
                 CONV_RULE (ONCE_DEPTH_CONV Target.sort_registers) |>
                 UNDISCH_ALL |>
                 (* Enable us to add constraints based on instruction locations *)
                 ADD_ASSUM ``instr_start ^(numSyntax.term_of_int n) = ^pc_s``

    in (th,mem_counter)
    end
    handle e => raise wrap_exn "CombineSteps" "add_prev_step" e

fun combine_steps' n [] = (TRUTH, 0)
  | combine_steps' n [th] = last_step n th
  | combine_steps' n (th::ths) =
    add_prev_step n th (combine_steps' (n+1) ths)

fun combine_steps ths = combine_steps' 0 (map Target.fixup_th ths)

end

(* Example

val [ex_th1] = m0_stepLib.thumb_step_hex (false,false) "4010";
val [ex_th2] = m0_stepLib.thumb_step_hex (false,false) "7740";
val [ex_th3] = m0_stepLib.thumb_step_hex (false,false) "4013";

(* Longer example, but illustrates that the last step to tidy updates
   isn't general enough. *)
val ex_th = CombineSteps.combine_steps [ex_th1,ex_th2,ex_th3];
*)
