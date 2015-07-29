signature Tools =
sig
  (* Reduce terms using some knowledge of the model's construction; the first
     version takes a list of constants to avoid reducing *)
  val mk_cbv_CONV : Term.term list -> Conv.conv
  val cbv_CONV : Conv.conv
  val cbv_eval : Term.term -> Term.term

  (* Apply a conversion to a term and return just the converted term *)
  val conv_term : Conv.conv -> Term.term -> Term.term

  (* Make an association-list-of-lists from a multi-association-list. *)
  val collect : (''a * 'b) list -> (''a * 'b list) list

  (* Turn ``f 3 = 5`` terms in (``f``, ``(3 =+ 5) f``) update terms. *)
  val eqns_to_updates : Term.term list -> (Term.term * Term.term) list

  val read_file : string -> string

  (* Merge and deduplicate two sorted lists (the comparison function does not
     need to be reflexive) *)
  val merge : (''a -> ''a -> bool) -> ''a list -> ''a list -> ''a list

  val random_word : RandGen.gen -> int -> int -> Term.term
end
