structure Harness =
struct
  datatype basic_harness = Breakpoint

  datatype harness =
    Basic of basic_harness
  | NOP_padding_after of int * basic_harness

fun basic_harness_to_src Breakpoint = "Breakpoint"

fun harness_to_src (Basic b) = "(Basic " ^ basic_harness_to_src b ^ ")"
  | harness_to_src (NOP_padding_after (n,b)) =
    "(NOP_padding_after (" ^ Int.toString n ^ "," ^ basic_harness_to_src b ^ "))"

(* NB: costs must stay in sync with implementation in StepSMT. *)

fun basic_harness_cost Breakpoint = 3

fun harness_cost (Basic b) = basic_harness_cost b
  | harness_cost (NOP_padding_after (n,b)) = n + basic_harness_cost b

fun basic_pc_offset Breakpoint = 0

fun pc_offset (Basic b) = basic_pc_offset b
  | pc_offset (NOP_padding_after (n,b)) = 2*n + basic_pc_offset b

fun harness_instrs h =
    let val is = case h of
                     Basic Breakpoint => [0xbe00]
                   | NOP_padding_after (nops,Breakpoint) =>
                     (List.tabulate (nops,fn _ => 0xbf00))@[0xbe00]
    in map (fn i => [i mod 256, i div 256]) is
    end

fun preamble_constraints () = []

end
