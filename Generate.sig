signature Generate = sig
  val test_instrs : int -> unit
  (* Returns hex code for instruction, string describing instruction format
     and the size of the instruction in bytes. *)
  val gen_one_instr : RandGen.gen -> string * string * int
  val gen_instrs : RandGen.gen -> int -> (string * string * int) list

  (* Return a sample instruction for each format (except for conditionals, where
     one per conditional is returned. *)
  val sample_instrs : RandGen.gen -> (string * string) list

(* Extracts a list of registers *chosen* during generation.
   This is for getting some idea of how well the register biasing is
   working, and is not necessary for generation. *)
  val regs_chosen_in_instr : (string * string * int) -> int list

end
