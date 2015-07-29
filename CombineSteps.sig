signature CombineSteps =
sig
  val NStates_def : Thm.thm
  (* Combine several step theorems into one theorem plus terms for the
     PCs for each instruction.  Introduces new assumptions of the form
     instr_start n = ... to give the start byte for each instruction and
     memory_access n = ... to give the address of every byte of memory
     accessed.  The number of elements in memory_access is returned. *)
  val combine_steps : Thm.thm list -> Thm.thm * int
end
