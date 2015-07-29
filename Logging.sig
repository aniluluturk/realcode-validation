signature Logging =
sig

(* A stateful module for recording test results. *)

(* Takes a directory name for the output, failing if it already exists, and a
   string to log in the summary (e.g., for extra constraints) *)
val setup : string -> string -> unit

(* So that we can start timing before calling new_test *)
type time
val start_time : unit -> time

(* Takes the output from using Generate.gen_instr lots, list of branch choices, and harness *)
val new_test : (string * string * int) list -> int list -> string -> time -> unit
(* val thm_choices : int list -> unit *)

(* Generators are stateful, and remember their output, so call this after all
   the random data has been generated. *)
val record_random : RandGen.gen -> unit

(* Record locations of instructions *)
val record_instr_locs : string -> unit

(* Split the individual test timing at this point (i.e., between phases) *)
val split_timing : unit -> unit

(* User and system times for SMT child processes *)
val smt_times : Time.time * Time.time -> unit

(* Produce a filename in the logging directory with the given prefix, suffix and
   an alternative to use if we're not in a test run. *)
val filename : string -> string -> string -> string

datatype complaint =
  Impossible_combination
| SMT_Unknown
| SMT_Unsat
| Mismatch of string
| Misc_exn of exn

val fail : complaint -> unit

val success : unit -> unit

val finish : unit -> unit

end
