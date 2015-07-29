use "tests.sml";

fun bin s = Option.valOf (StringCvt.scanString (Int.scan StringCvt.BIN) s);
fun hex s = Option.valOf (StringCvt.scanString (Int.scan StringCvt.HEX) s);

exception BadChar of char;

fun mask_char #"0" = #"1"
  | mask_char #"1" = #"1"
  | mask_char #"_" = #"0"
  | mask_char c = raise (BadChar c)

fun val_char #"0" = #"0"
  | val_char #"1" = #"1"
  | val_char #"_" = #"0"
  | val_char c = raise (BadChar c)

fun mask s = bin (String.map mask_char s)
fun bval s = bin (String.map val_char s)
fun matches i s =
    let val m = mask s
	val v = bval s
    in IntInf.andb (m,i) = v
    end

fun is_store instr =
    List.exists (matches instr)
		["1011010_________",
		 "11000___________",
		 "01100___________",
		 "10010___________",
		 "0101000_________",
		 "01110___________",
		 "0101010_________",
		 "10000___________",
		 "0101001_________"]

fun is_unaligned pc = (pc mod 4) <> 0

fun correct (instrs, model, measurement, pcs) =
    let val instrs = map (fn (i,_,_) => hex i) instrs
        fun correct (i,pc,(count,fixed_already)) =
            let val fix_align = is_unaligned pc andalso is_store i
                val eom = pc >= 0x20000FFC
                val fix_eom = eom andalso not fixed_already
                val correction = if fix_align orelse fix_eom then 1 else 0
            in (count+correction, eom orelse fix_align)
            end
	val (corrected,_) = ListPair.foldl correct (model,false) (instrs,pcs)
    in if corrected = measurement
       then (print "OK\n"; NONE)
       else (print "Bad\n"; SOME (instrs, model, measurement, pcs, corrected))
    end;

val bad = List.mapPartial correct mismatches;


fun unaffected (instrs, pcs) =
    let val instrs = map (fn (i,_,_) => hex i) instrs
	val ok = ListPair.all (fn (i,pc) => not (is_unaligned pc andalso is_store i))
			      (instrs,pcs)
    in if ok then print "OK\n" else print "Bad\n"
    end;

List.app unaffected successes;


print "Failed corrections:\n";
app (fn (instrs,_,_,pcs,_) => (ListPair.app (fn (i,n) => print (Int.fmt StringCvt.HEX i ^ " " ^ Int.fmt StringCvt.HEX n ^ " ")) (instrs,pcs); print "\n")) bad;
