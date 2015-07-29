structure State :> State=
struct

open HolKernel Parse boolLib
open RandGen

val background_memory = mk_var ("background_memory", Target.memory_type)

fun random_byte gen =
    let val i = genint gen 255
    in wordsSyntax.mk_wordii (i, 8)
    end

fun fill_memory gen start len =
    let val bytes = List.tabulate (len, fn _ => random_byte gen)
        val hol_bytes = listSyntax.mk_list (bytes, ``:word8``)
        val start = wordsSyntax.mk_wordii (start,Target.addr_size)
        val len = wordsSyntax.mk_wordii (len,Target.addr_size)
    in ``\i. if (i >= ^start /\ i < ^start + ^len) then EL (w2n (i - ^start)) ^hol_bytes else ^(background_memory) i``
    end

fun fill_memory_partial_hol gen start len locations =
    let val bytes = Vector.tabulate (len, fn _ => genint gen 255)
        val w_bytes = Vector.map (fn i => wordsSyntax.mk_wordii (i, 8)) bytes
        val hol_bytes =
            foldl (fn (i,m) => ``((^(wordsSyntax.mk_wordii (i, Target.addr_size)) =+
                                   ^(Vector.sub (w_bytes, i-start))) ^m)``)
                  background_memory locations
    in ([(start,bytes)],hol_bytes)
    end

fun fill_some_memory gen locations =
    foldl (fn (i,m) => ``((^(wordsSyntax.mk_wordii (i, Target.addr_size)) =+ ^(random_byte gen)) ^m)``)
          background_memory locations

fun get_tm S v = Option.valOf (subst_assoc ((equal v) o fst o dest_var) S)

fun insert_update (start,content) =
    let val len = Vector.length content
        fun aux [] = [(start,content)]
          | aux (m as ((start',content')::t)) =
            let val len' = Vector.length content' in
            (* Non-interfering *)
            if start + len < start' then (start,content)::m else
            if start > start' + len' then (start',content')::(aux t) else
            (* Completely overrides *)
            if start <= start' andalso start + len > start' + len' then aux t else
            (* Restart with merged block (start > start' by above) *)
            if start + len > start' + len' then
                insert_update (start',
                               VectorSlice.concat [VectorSlice.slice (content', 0, SOME (start-start')),
                                                   VectorSlice.full content]) t
            (* Collides with front of block *)
            else if start <= start' then
               (start,
                VectorSlice.concat [VectorSlice.full content,
                                    VectorSlice.slice (content', len-(start'-start), NONE)])::t
            (* Embedded within (start > start' by above) *)
            else (start',
                  VectorSlice.concat [VectorSlice.slice (content', 0, SOME (start-start')),
                   VectorSlice.full content,
                   VectorSlice.slice (content', start-start'+len, NONE)])::t
            end
    in aux
    end

(*

fun fl l = Vector.fromList l

val test = [(0,fl [0,1,2,3]), (6,fl [6,7]), (10,fl [10,11,12,13,14]), (20,fl [20]), (23,fl [23])];
insert_update (8,fl [8]) test;
insert_update (9,fl [9]) test;
insert_update (8,fl [8,9]) test;
insert_update (5,fl [5,6,7,8]) test;
insert_update (5,fl [5,6]) test;
insert_update (5,fl [5,6,7,8,9]) test;
insert_update (5,fl [5,6,7,8,99,1010]) test;
insert_update (7,fl [7,8]) test;
insert_update (7,fl [7,8,9]) test;
insert_update (7,fl [7,8,99,1010]) test;
insert_update (19,fl [19,20,21]) test;
*)

fun decompose_hol_memory m bkgd =
    case Lib.total (match_term ``((addr:^(ty_antiq Target.word_type) =+ (value:word8)) memory)``) m of
      SOME (S,T) =>
      let val addr = get_tm S "addr"
          val addr = wordsSyntax.uint_of_word addr
          val value = get_tm S "value"
          val value = wordsSyntax.uint_of_word value
          val m' = get_tm S "memory"
          val m' = decompose_hol_memory m' bkgd
      in insert_update (addr,Vector.fromList [value]) m'
      end
    | NONE =>
      (* The term may have been reduced, so this isn't exactly the same as the
         term that produced it. *)
      case Lib.total (match_term ``\i:^(ty_antiq Target.word_type).
           if (i >= start /\ i < end)
           then EL (w2n (i + start')) content
           else memory i : word8``) m of
        SOME (S,T) =>
        let val start = get_tm S "start"
            val start = wordsSyntax.uint_of_word start
            val content = get_tm S "content"
            val content = (fst o listSyntax.dest_list) content
            val content = map wordsSyntax.uint_of_word content
            val m' = get_tm S "memory"
            val m' = decompose_hol_memory m' bkgd
        in insert_update (start, Vector.fromList content) m'
        end
      | NONE =>
        if m = background_memory then  bkgd
        else failwith ("Unparsable memory: " ^ term_to_string m)

(* TODO: this shouldn't need to keep track of seen memory any more *)
fun fold_memory memory bkgd f x =
    let val memory = decompose_hol_memory memory bkgd
        fun aux ((start,content),(seen,x)) =
            let val x' =
                    Vector.foldli (fn (off,b,x) =>
                                     let val addr = start+off
                                     in if seen addr then x else f addr b x
                                     end) x content
            in (fn addr => if addr >= start andalso addr < start+Vector.length content
                     then true else seen addr, x')
            end
        val (_,r) = List.foldl aux (fn _ => false, x) memory
    in r
    end handle (e as HOL_ERR _) => raise (wrap_exn "State" "fold_memory" e)
end
