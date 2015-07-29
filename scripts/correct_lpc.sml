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
	val corrected = ListPair.foldl
			    (fn (i,pc,count) =>
				if is_unaligned pc andalso is_store i
				then count + 1
				else count) model (instrs,pcs)
    in if corrected = measurement then print "OK\n" else print "Bad\n"
    end;

List.app correct mismatches;


fun unaffected (instrs, pcs) =
    let val instrs = map (fn (i,_,_) => hex i) instrs
	val ok = ListPair.all (fn (i,pc) => not (is_unaligned pc andalso is_store i))
			      (instrs,pcs)
    in if ok then print "OK\n" else print "Bad\n"
    end;

List.app unaffected successes;

