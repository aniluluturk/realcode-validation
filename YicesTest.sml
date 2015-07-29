(* A script to find everything that could end up in an SMT problem that the
   Yices translation doesn't support. *)

fun field_updates ty =
  let val ty_name = fst (dest_type ty)
      val fields = TypeBase.fields_of ty
      val sub = map (fn (_,ty) => if TypeBase.is_record_type ty then field_updates ty else []) fields
      val ups = map (fn (field,_) => ty_name ^ "_" ^ field ^ "_fupd") fields
  in List.concat (ups::sub)
  end

(* Constants that will appear in the goals we generate here, but not (hopefully) in
   preconditions.  Some record updates can be handled by the Yices translation, but
   there's something about the structure of the ones here that it doesn't like;
   they're also unlikely to appear in preconditions, however.  *)
val ignorable = ["ARB","UPDATE","K",fst (dest_const Target.nextstate)] @
                (field_updates Target.state_type);

val gen = RandGen.newgen ();

val instrs = Generate.sample_instrs gen;

(* Alternative:

fun rand_hex gen n =
    let val digits = [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9",
                      #"a", #"b", #"c", #"d", #"e", #"f"]
      val ds = List.tabulate (n,fn _ => snd (RandGen.select digits gen))
  in String.implode ds
  end

(* Use 4 for thumb4 *)
val instrs = List.tabulate (1000,fn _ => (rand_hex gen 8, ""));
*)

val ths = List.concat (map (fn (h,n) => map (fn th => (th,(h,n))) ((Target.step h) handle e => []))
                           instrs);
val ths = map (fn (th,n) => (StepSMT.trans_thm_for_test
                                (Target.fixup_th
                                    (StepSMT.early_rewrites th)), n)) ths;
fun find_uninterp (th,name) =
    let
        val (_,_,_,uninterp) = YicesSat.goal_to_Yices (dest_thm th)
                               handle e => (print (fst name ^ " " ^ snd name ^ "\n");
                                            raise e)
        val uninterp = List.filter (fn tm => not (is_const tm andalso
                          mem ((fst o dest_const) tm) ignorable)) uninterp
    in (uninterp,name)
    end;
val uninterps = map find_uninterp ths;
val bad = List.filter (fn (l,_) => l <> []) uninterps;

(* Some useful bits and pieces *)

map (fn (a,(h,n)) => (a,h,n,Target.step h)) bad;

(* Check out ror definition in YicesSat works for at least one non-trivial example *)

YicesSat.Yices_sat ([], ``n > 5 /\ n < 32 /\ (w #>> n = (14w:word32))``);
YicesSat.Yices_sat ([], ``n > 5 /\ (w ' 0) /\ (w #>> n = (14w:word32))``);
YicesSat.Yices_sat ([], ``n > 45 /\ (w ' 0) /\ (w #>> n = (14w:word32))``);
