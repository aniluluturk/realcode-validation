signature StepSMT =
sig

  (* Raised when we notice that the preconditions are unsatisfiable during
     transformation, before calling the solver. *)
  exception Preconditions_impossible

  (* Rewrites to apply early on to prevent evaluation steps from blocking them
     later *)
  val early_rewrites : Thm.thm -> Thm.thm

  (* Terms that should not be evaluated because they're supported by the SMT
     translation but might reduce to something that's not. *)
  val non_eval_terms : Term.term list

  (* Additional constraints that will be added; default to true *)
  val no_self_modification_flag : bool ref
  val describe_additional : unit -> string

  (* Produce an SMT-friendly precondition for the given step (or
     combined steps) theorem, a list of the instruction lengths, the
     size of memory_access and a list of extra constraints. *)

  val trans_thm : Thm.thm -> int list -> int -> Harness.harness -> Term.term list -> Abbrev.goal

  (* Perform the main simplification steps of trans_thm, but leave in the
     conclusion so that we can test the SMT translation on any terms that
     *might* appear in some combination of instructions. *)
  val trans_thm_for_test : Thm.thm -> Thm.thm

  (* Take some assignments (after Tools.eqns_to_updates) and augment
     a state with them *)
  val augment_state : Term.term -> (Term.term * Term.term) list -> Term.term

  val instr_starts : (Term.term * Term.term) list -> Term.term
  val memory_addresses : (Term.term * Term.term) list -> Term.term
  val write_addresses : (Term.term * Term.term) list -> Term.term

end
