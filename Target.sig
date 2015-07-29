(* Basic definitions go into TargetBasics, so that Target can rely on Tools. *)

signature Target =
sig

val sram_start : int
val sram_size : int

(* Triples of address, value and mask to write during device initialisation *)
val init : (int * int * int) list

(* Step function of the model *)
val nextstate : Term.term
(* PC term for a given state *)
val pc : Term.term -> Term.term

val memory_field_update : Term.term
val memory_type : Type.hol_type
val sort_addresses : Conv.conv
val sort_registers : Conv.conv
val state_type : Type.hol_type

(* Clean up minor details of step theorems from stepLib *)
val fixup_th : Thm.thm -> Thm.thm

val word_size : int
val word_type : Type.hol_type
val addr_size : int

val basic_state : Term.term

(* [fill_in_state gen state mem] returns the [state] with the *unspecified*
   parts of memory replaced by [mem] and unspecified registers and flags
   chosen by consulting the random number source, [gen].  Assumes the free
   variables used by [basic_state] are used for unspecified values in [state].
 *)
val fill_in_state : RandGen.gen -> Term.term -> Term.term -> Term.term

(* Describe/add optional additional constraints which are target-specific.
   Flags controlling these are added to the signature on a per-target
   basis (see the M0 one for an example). *)
val describe_additional : unit -> string
val additional_constraints : Thm.thm -> (Term.term -> Term.term) -> Term.term list

val smt_rewrites : Thm.thm list
val smt_simps : Thm.thm list

(* Generate a step theorem for an instruction given in hex *)
val step : string -> Thm.thm list

(* May depend upon whether the model's tools support them *)
val print_disassembled_hex : string -> unit
val encode : string quotation -> (string * int) list (* hex and instr size *)
val step_code : string quotation -> Thm.thm list

(* Simple theorem to handle splitting up the fields of the state record *)
val split_state_thm : Thm.thm

(* Choose which step theorem to use for each instruction (i.e., pick whether to
   take branches) *)
val choose_thms : RandGen.gen -> Thm.thm list list -> int list

end
