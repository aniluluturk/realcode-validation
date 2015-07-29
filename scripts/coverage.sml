use "instrs.sml";

fun insert (x:string) [] = [x]
  | insert x (h::t) = if x < h then x::h::t else h::(insert x t)

fun sort [] = []
  | sort (h::t) = insert h (sort t)

fun dedup (x::y::t) = if x = y then dedup (y::t) else x::(dedup (y::t))
  | dedup l = l

local
    fun more n x (y::t) = if x = y then more (n+1) x t else (n, t)
      | more n x [] = (n, [])
in
fun count [] = []
  | count (h::t) =
    let val (n, t') = more 1 h t
    in (h,n)::(count t')
    end
end

fun min [x] = (x:string * int)
  | min (h::t) = let val m = min t in if #2 m < #2 h then m else h end

fun max [x] = (x:string * int)
  | max (h::t) = let val m = max t in if #2 m > #2 h then m else h end

val formats = map (map #2) instrs;

val single_formats = map (dedup o sort) formats;

val patterns_format_appears_in = (count o sort o List.concat) single_formats;

fun str_pair (s,i) = Int.toString i ^ " " ^ s ^ "\n";
( List.app (print o str_pair) patterns_format_appears_in
; print ("Total number of instruction formats used: " ^
	 Int.toString (length patterns_format_appears_in) ^
	 "\n")
; print ("min: " ^ str_pair (min patterns_format_appears_in))
; print ("max: " ^ str_pair (max patterns_format_appears_in))
);
