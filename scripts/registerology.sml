(*use "instrs.sml";*)

val regs = map (map (Generate.regs_chosen_in_instr)) instrs;

val regs = map (map (sort (fn x => fn y => x < y))) regs;

fun reuse [] = false
  | reuse [_] = false
  | reuse (x::y::t) = x = y orelse reuse (y::t)

val intra_instr_reuse = List.concat (map (map reuse) regs);

fun rate l =
    let fun aux (false, (total,count)) = (total+1,count)
	  | aux (true,  (total,count)) = (total+1,count+1)
	val (total, count) = foldl aux (0,0) l
    in (Real.fromInt count) / (Real.fromInt total)
    end

val intra_instr_reuse_rate = rate intra_instr_reuse;

fun dedup [] = []
  | dedup [x] = [x]
  | dedup (x::y::t) = if x = y then dedup (y::t) else x::(dedup (y::t))

val test_regs = map (sort (fn x => fn y => x < y) o List.concat o map dedup) regs;

(* Simple yes/no isn't very interesting *)
val inter_instr_reuse = map reuse test_regs;
val inter_instr_reuse_rate = rate inter_instr_reuse;

fun count_dup [] = 0
  | count_dup [x] = 0
  | count_dup (x1::x2::xs) =
    let fun drop [] = []
	  | drop (y::ys) = if x1 = y then drop ys else y::ys
    in if x1 = x2 then 1+(count_dup (drop xs)) else count_dup (x2::xs)
    end

fun mean l =
    let val total = foldl (op +) 0 l
    in (Real.fromInt total) / (Real.fromInt (length l))
    end

(* The number of registers reused in each test *)
val inter_instr_reuse_count = map count_dup test_regs;
val inter_instr_reuse_mean = mean inter_instr_reuse_count;

( print ("Rate of instructions which reuse a register: " ^ Real.toString intra_instr_reuse_rate ^ "\n")
; print ("Rate of tests which reuse a register: " ^ Real.toString inter_instr_reuse_rate ^ "\n")
; print ("Mean of registers reused in each test: " ^ Real.toString inter_instr_reuse_mean ^ "\n")
);
