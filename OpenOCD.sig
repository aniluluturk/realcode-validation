signature OpenOCD =
sig
  type connection
  val connect : unit -> connection
  val close : connection -> unit
  val send_command : connection -> string -> string
  (* Fails if the response isn't empty *)
  val send_quiet_command : connection -> string -> unit

  (* Registers; named by strings *)

  val set_reg : connection -> string -> int -> unit
  val get_register : connection -> string -> int

  (* Memory *)

  val mwb : connection -> int -> int -> unit
  val set_memory : connection -> int -> int vector -> unit
  val get_byte : connection -> int -> int
  val get_word : connection -> int -> int
  val get_memory : connection -> int -> int -> int array
end
