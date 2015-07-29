(* Main testing code *)

structure Test =
struct

local
open HolKernel boolLib bossLib
in

fun report_instrs is =
    let val _ = print ("Instructions: \n")
    in List.app (fn (x,_,_) => Target.print_disassembled_hex x) is
    end

fun report_choices cs =
    let val cs = map Int.toString cs
        val s = String.concatWith "," cs
    in print ("Branch choices: [" ^ s ^ "]\n")
    end

fun generate gen n =
    let val () = print "Generating instructions:\n"
        val instrs = Generate.gen_instrs gen n
    in instrs
    end

exception Combination_impossible

fun choose gen instrs choices =
    let val () = print "Choosing step theorems.\n"
        val thmss = map (fn (hex,_,_) => Target.step hex) instrs
        val choices = 
            case choices of
                NONE => Target.choose_thms gen thmss
              | SOME cs => cs
    in
       (choices,ListPair.map (fn (c,thms) =>
                                 StepSMT.early_rewrites (List.nth (thms, c)))
                             (choices,thmss))
    end

fun combine ths =
    let val () = print "Combining step theorems.\n"
        val (th, mem_counter) = CombineSteps.combine_steps ths
        (* If the theorems are incompatible, for example because a flag is set
           to the opposite of what a branch requires, then the conclusion will
           have been simplified to true. *)
        val () = if concl th = ``T``
                 then raise Combination_impossible
                 else ()
    in (th, mem_counter)
    end

(* Instantiate all of the free variables that have a definition in the
   hypotheses; in particular the state and mem_step_n variables.
   There is some crude dependency analysis to try to keep terms
   relatively concrete, hopefully avoiding blow-ups due to alias
   checking.  (Maybe a proper topological sort would be better.)
 *)
fun instantiate_free_var_hyp th =
    let open optionSyntax
        fun extract_def tm =
            if markerSyntax.is_abbrev tm
            then let val (l,r) = markerSyntax.dest_abbrev tm
                 in SOME (mk_var (l, type_of r), r)
                 end
            else if is_eq tm
            then if is_var (lhs tm)
                 then SOME (lhs tm, rhs tm)
                 else if is_var (rhs tm)
                 then SOME (rhs tm, lhs tm)
                 else if is_some (lhs tm) andalso is_some (rhs tm)
                 then extract_def (mk_eq (dest_some (lhs tm), dest_some (rhs tm)))
                 else NONE
            else if is_comb tm andalso rator tm = ``bool$~`` andalso is_var (rand tm)
            then SOME (rand tm, ``F``)
            else NONE
        val defs = List.mapPartial extract_def (hyp th)
        val vars = map fst defs
        val defs = List.filter (fn (_,r) => not (List.exists (fn v => mem v vars) (free_vars r))) defs
        val inst = map (fn (l,r) => {redex = l, residue = Tools.cbv_eval r}) defs
    (* I suspect this might be a little expensive, but reducing here may put
       some hypotheses into a form where they can be used *)
    in if inst = [] then th else instantiate_free_var_hyp (INST inst th |> DISCH_ALL |> Conv.CONV_RULE Tools.cbv_CONV |> UNDISCH_ALL)
    end


fun instantiate_th th s instr_start memory_addresses =
    let val th = DISCH_ALL th
        (* We transform the theorem to refer to individual parts of the state
           before instantiating the real state to avoid duplicating the full
           memory too much. *)
        val st_th =
            Target.split_state_thm |>
                      INST [{redex=``P:^(ty_antiq Target.state_type) -> bool``,
                             residue=``\s. ^(concl th)``}] |>
                      CONV_RULE (RATOR_CONV (RAND_CONV BETA_CONV));
        val th = 
            th |>
               MP st_th |>
               (* Note that only the hypotheses from state_thm move, the "real"
                  ones will still be in the conclusion until we reduce it to get
                  rid of all mention of the state record. *)
               UNDISCH_ALL |>
               (* I used EVAL_CONV here previously, but this exposes details from n-bit
                  that subsequent uses of cbv_CONV won't handle *)
               CONV_RULE Tools.cbv_CONV |>

               (* Instantiate the state and clear up projections by reduction *)
               INST [{redex = mk_var ("s", Target.state_type), residue = s},
                     {redex = ``instr_start:num -> ^(ty_antiq Target.word_type)``, residue = instr_start},
                     {redex = ``memory_address:num -> ^(ty_antiq Target.word_type)``, residue = memory_addresses}] |>
               DISCH_ALL |>
               CONV_RULE Tools.cbv_CONV |>

               (* Push all the definitions in the hypotheses into the rest of
                  the theorem *)
               UNDISCH_ALL |>
               instantiate_free_var_hyp |>
               DISCH_ALL |>
               PURE_REWRITE_RULE [markerTheory.Abbrev_def] |>

               (* Clean everything up *)
               CONV_RULE Tools.cbv_CONV |>
               CONV_RULE (ONCE_DEPTH_CONV Target.sort_addresses) |>
               CONV_RULE (ONCE_DEPTH_CONV Target.sort_registers) |>
               UNDISCH_ALL
    in if hyp th <> []
       then let val s = String.concatWith "\n" (map term_to_string (hyp th))
                val s = if String.size s > 1024 then String.substring (s,0,1020) ^ "..." else s
            in failwith ("Pre-state did not satisfy all hypotheses:\n" ^ s)
            end
       else th
    end

local
    fun aux 0 _ = []
      | aux n instr_start =
        (term_to_string (Tools.cbv_eval ``^instr_start ^(numSyntax.term_of_int (n-1))``))::
        (aux (n-1) instr_start)
in
fun instr_start_string n instr_start =
    (String.concatWith "," o rev) (aux n instr_start)
end

val last_random_trace = ref ""

open Harness

datatype instr_source = Generate of int | Manual of (string * string * int) list

(* There's a version of this with detailed logging in logged_test. *)
fun construct_test gen instrs choices harness constraints =
    let
        val gen = case gen of SOME g => RandGen.replay g | NONE => RandGen.newgen ()
        val (n,instrs) = case instrs of Manual is => (length is, is)
                                      | Generate n => (n, generate gen n)
        val () = report_instrs instrs
        val (choices, ths) = choose gen instrs choices
        val () = report_choices choices
        val (th, mem_counter) = combine ths
        val () = print "Attempting to find pre-state.\n"
        val (s, instr_start, read_footprint, write_footprint) =
            Prestate.mk_prestate th instrs mem_counter harness constraints
(*      val bare_th = instantiate_th th s instr_start read_footprint *)
        (* Use n+1 so that we report where the harness starts *)
        val () = print ("Instruction locations: " ^ (instr_start_string (n+1) instr_start) ^ "\n")
    in (gen, instrs, s, th, instr_start, read_footprint, write_footprint)
    end

fun run_test debug gen0 instrs choices harness constraints =
    let
        val (gen, instrs, s, th, instr_start, read_footprint, write_footprint) =
            construct_test gen0 instrs choices harness constraints
        val read_footprint' = sort (fn x => fn y => x < y) (Prestate.extract_footprint read_footprint)
        val write_footprint' = sort (fn x => fn y => x < y) (Prestate.extract_footprint write_footprint)
        val footprint = Tools.merge (fn x => fn y => x < y) read_footprint' write_footprint'
        val (bgmem, s) = Prestate.fill_in_state gen s footprint
        val () = last_random_trace := RandGen.record gen
        val () = print "Instantiating combined step theorem with state.\n"
        val full_th = instantiate_th th s instr_start read_footprint

        val () = print "Running hardware test and comparing:\n"
        val _ = HWTest.run_and_check debug bgmem s full_th harness
        val () = case gen0 of NONE => RandGen.dispose gen | _ => ()
    in ()
    end

(* Simple version which takes assembly *)
fun run_test_code debug code choices harness =
    let val instrs = Target.encode code
        val instrs = map (fn (h,n) => (h, "", n)) instrs
    in run_test debug NONE (Manual instrs) choices harness
    end

fun construct_test_code code choices harness =
    let val instrs = Target.encode code
        val instrs = map (fn (h,n) => (h, "", n)) instrs
    in construct_test NONE (Manual instrs) choices harness
    end

local
    val pre_times = ref NONE
in
fun record_smt () =
    let val post_times = Posix.ProcEnv.times ()
        val pre_times = Option.valOf (!pre_times)
        val smt_times = (Time.- (#cutime post_times, #cutime pre_times),
                         Time.- (#cstime post_times, #cstime pre_times))
    in Logging.smt_times smt_times
    end

fun logged_test0 debug n harness constraints gen =
    let
        val start_time = Logging.start_time ()
        val instrs = generate gen n
        val (choices, ths) = choose gen instrs NONE
        val () = Logging.new_test instrs choices (harness_to_src harness) start_time
        val () = report_instrs instrs
        val () = Logging.split_timing ()
        val (th, mem_counter) = combine ths
        val () = Logging.split_timing ()
        val () = print "Attempting to find pre-state.\n"
        (* We assume that the only active children are for the SMT solver *)
        val () = pre_times := SOME (Posix.ProcEnv.times ())
        val (s, instr_start, read_footprint, write_footprint) =
            Prestate.mk_prestate th instrs mem_counter harness constraints
        val () = record_smt ()
        (* Use n+1 so that we report where the harness starts *)
        val () = Logging.record_instr_locs (instr_start_string (n+1) instr_start)
        val read_footprint' = sort (fn x => fn y => x < y) (Prestate.extract_footprint read_footprint)
        val write_footprint' = sort (fn x => fn y => x < y) (Prestate.extract_footprint write_footprint)
        val footprint = Tools.merge (fn x => fn y => x < y) read_footprint' write_footprint'
        val (bgmem, s) = Prestate.fill_in_state gen s footprint
        val () = Logging.record_random gen
        val () = Logging.split_timing ()
        val () = print "Instantiating combined step theorem with state.\n"
        val full_th = instantiate_th th s instr_start read_footprint
        val () = Logging.split_timing ()

        val () = print "Running hardware test and comparing:\n"
    in case HWTest.run_and_check debug bgmem s full_th harness of
           NONE => Logging.success ()
         | SOME detail => Logging.fail (Logging.Mismatch detail)
    end
    (* The difference between the first two is rather technical and unimportant *)
    handle Combination_impossible => Logging.fail Logging.Impossible_combination
         | StepSMT.Preconditions_impossible => Logging.fail Logging.Impossible_combination
         | Prestate.SMT_Unknown => (record_smt (); Logging.fail Logging.SMT_Unknown)
         | Prestate.SMT_Unsat => (record_smt (); Logging.fail Logging.SMT_Unsat)
           (* The Interrupt clause doesn't seem to have any effect, but I don't know why... *)
         | Interrupt => (Logging.fail (Logging.Misc_exn Interrupt); raise Interrupt)
         | e => Logging.fail (Logging.Misc_exn e)
end

fun logged_test debug n harness constraints =
    let val gen = RandGen.newgen ()
        val () = logged_test0 debug n harness constraints gen
    in RandGen.dispose gen
    end

fun iter (f:'a -> unit) x 0 = ()
  | iter f x n = (f x; iter f x (n-1))

fun logged_run dir debug n harness count constraints =
    let val constraints_str = String.concatWith ", " (map term_to_string constraints)
        val () = HWTest.reset debug (* Just in case it's in a strange state before CPU id *)
        val cpu_info = HWTest.cpu_id debug
        val () = Logging.setup dir
                               ("Additional constraints: " ^ StepSMT.describe_additional () ^
                                "\nAdditional user constraints: " ^ constraints_str ^ "\n" ^
                                "CPU ID\n" ^ cpu_info)
        val () = (iter (logged_test debug n harness) constraints count) handle Interrupt => ()
    in Logging.finish ()
    end

end

end
