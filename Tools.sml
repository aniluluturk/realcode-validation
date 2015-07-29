structure Tools : Tools =
struct

local
open HolKernel Parse boolLib
in

(* For general call-by-value computation, except for a given list of terms. *)
fun mk_cbv_CONV non_eval =
    let val cmp = TargetBasics.compset ()
        val () = 
            computeLib.add_conv (bitstringSyntax.v2w_tm, 1, bitstringLib.v2w_n2w_CONV)
                                cmp
        val () = computeLib.add_thms [wordsTheory.word_srem_def] cmp
        val () = List.app (fn t => (computeLib.scrub_const cmp t) handle NotFound => ()) non_eval
    in computeLib.CBV_CONV cmp
    end

fun conv_term conv tm = (snd o dest_eq o concl o conv) tm

(* Most of the time we don't need to restrict anything. *)
val cbv_CONV = mk_cbv_CONV []
val cbv_eval = conv_term cbv_CONV

fun collect l =
    let fun add k v [] acc = (k,[v])::acc
          | add k v ((k',vs)::t) acc =
            if k = k' then ((k,v::vs)::t)@acc else add k v t ((k',vs)::acc)
        fun aux [] l = l
          | aux ((k,v)::tl) l = aux tl (add k v l [])
    in aux l []
    end
            

fun eqns_to_updates tms =
    let fun up [] vl _ = vl
          | up (arg::args) vl old =
            ``(^arg =+ ^(up args vl (mk_comb (old,arg)))) ^old``
        fun f tm =
            let val (lhs,rhs) = dest_eq tm
                val (var,args) = strip_comb lhs
            in (var, up args rhs var)
            end
        val updates = map f tms
        val updates = collect updates
        fun merge (v,l) = (v,List.foldl (fn (u,us) => subst [{redex = v, residue = u}] us) v l)
    in map merge updates
    end

fun read_file filename =
    let val ins = TextIO.openIn filename
        val s = TextIO.inputAll ins
        val () = TextIO.closeIn ins
    in s
    end

local
    fun drop x [] = []
      | drop x (y::ys) = if x = y then drop x ys else y::ys
in
(* Merge and deduplicate two sorted lists *)
fun merge cmp [] ys = ys
  | merge cmp xs [] = xs
  | merge cmp (x::xs) (y::ys) =
    if cmp x y orelse x = y then x::(merge cmp (drop x xs) (drop x (y::ys)))
    else y::(merge cmp (x::xs) (drop y ys))
end

(* Rather indirect, but never mind. *)
fun random_word gen n zero_bits =
    let val bits = RandGen.bits gen n @ List.tabulate (zero_bits, fn _ => false)
        val hbits = bitstringSyntax.bitstring_of_bitlist bits
        val ty = wordsSyntax.mk_int_word_type (n+zero_bits)
        val word = ``v2w ^hbits : ^(ty_antiq ty)``
    in (snd o dest_eq o concl o bitstringLib.v2w_n2w_CONV) word
    end

end

end
