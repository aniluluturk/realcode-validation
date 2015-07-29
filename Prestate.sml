structure Prestate = struct

local
open HolKernel boolLib
in

exception SMT_Unknown
exception SMT_Unsat

(* Remove any equations where the function name is in the bad list. *)
fun filter_eqns bad l =
    let fun is_good tm =
            let val var = (fst o strip_comb o fst o dest_eq) tm
            in is_var var orelse not (List.exists (fn x => (fst o dest_const) var = x) bad)
            end
    in List.filter is_good l
    end

(* val (step_th, instrs) = (ex_th,ex_instrs); *)

fun mk_prestate step_th instrs mem_counter harness constraints  =
    let val hol_precond =
            StepSMT.trans_thm step_th (map (fn (_,_,len) => len) instrs) mem_counter harness constraints
    in case YicesSat.Yices_sat hol_precond of
           YicesSat.SAT ass =>
           let  (* val ass = filter_eqns ["w2n", "word_bit"] ass*)
               val ass = Tools.eqns_to_updates ass
                         
               (* Use free variables for base registers and memory so that we can get a
                  readable result then backfill actual values. *)
               val s = StepSMT.augment_state (Target.basic_state) ass
               (* This will be a little too redundant, reduce it. *)
               val s = Tools.cbv_eval s
               val instr_start = StepSMT.instr_starts ass
               val read_footprint = StepSMT.memory_addresses ass
               val write_footprint = StepSMT.write_addresses ass
           in (s, instr_start, read_footprint, write_footprint)
           end
         | YicesSat.UNSAT => raise SMT_Unsat
         | YicesSat.UNKNOWN => raise SMT_Unknown
    end

datatype memory_opt = FullHOL | FullSML | Partial

val full_memory_option = ref FullHOL

fun fill_in_state gen s footprint =
    let val (sml_mem, rand_mem) =
            case !full_memory_option of
                FullHOL => ([], State.fill_memory gen Target.sram_start Target.sram_size)
              | FullSML => State.fill_memory_partial_hol gen Target.sram_start Target.sram_size footprint
              | Partial => ([], State.fill_some_memory gen footprint)
    in (sml_mem, Target.fill_in_state gen s rand_mem)
    end

(* Turn a HOL-formatted footprint into a plain list of int addresses *)
fun extract_footprint fp =
    let val (fp, terminal) = combinSyntax.strip_update fp
        val () = if is_arb terminal
                 then ()
                 else failwith ("Malformed footprint: ends with " ^ term_to_string terminal)
    in map (wordsSyntax.uint_of_word o snd) fp
    end

end

end
