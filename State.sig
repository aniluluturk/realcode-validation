signature State =
sig
  (* Free variable representing any memory not specified after
     fill(_some)_memory *)
  val background_memory : Term.term

  (* Produce an area of memory with random bytes (gen, start, length), background_memory
     everything else *)
  val fill_memory : RandGen.gen -> int -> int -> Term.term

  (* Produce an area of memory with random bytes (gen, start, length), background_memory
     everything else; putting all the bytes into the SML list and only the footprint
     into the HOL term *)
  val fill_memory_partial_hol : RandGen.gen -> int -> int -> int list -> ((int * int vector) list) * Term.term

  (* Produce a term for memory with a random byte at every address in the
     given list of locations *)
  val fill_some_memory : RandGen.gen -> int list -> Term.term

  (* Return the contents of memory as a list of nonoverlapping (start,
     content) pairs ordered by start location; some background memory can be
     supplied that is used if nothing in the term overrides it. *)
  val decompose_hol_memory : Term.term -> (int * int vector) list -> (int * int vector) list

  (* Call a function for each byte of the given memory; order not guaranteed *)
  val fold_memory : Term.term -> (int * int vector) list -> (int -> int -> 'a -> 'a) -> 'a -> 'a
end
