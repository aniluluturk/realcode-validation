(* Some definitions useful for the instruction generators *)

structure GenerationTools = struct

open Lib Feedback

local open RandGen in

(* Select n elements from a list, allowing duplicates to influence probability
   but *not* to appear multiple times in the output. *)
local
    fun one g l =
        let val n = length l
        in snd (select l g)
        end
    fun remove x = List.filter (fn y => x <> y)
in
fun select_n g _ 0 = []
  | select_n g [] _ = raise Empty
  | select_n g l n =
    let val h = one g l
        val t = remove h l
    in h::(select_n g t (n-1))
    end
end;

fun weighted_select l = select (List.concat (map (fn (n,x) => List.tabulate (n,fn _ => x)) l))

(* Helper functions for generating bits. *)

fun tbl l = map (fn x => x = 1) l

fun eor false false = false
  | eor false  true = true
  | eor  true false = true
  | eor  true  true = false

local
    fun f 0 = []
      | f n = (n mod 2 = 1)::(f (n div 2))
in
fun to_bits i =
    if i < 0 then failwith ("to_bits given a negative number: " ^ Int.toString i)
    else rev (f i)
fun to_n_bits n i = 
    let val bs = to_bits i
        val ln = length bs
    in if ln > n
       then failwith ("Can't fit " ^ Int.toString i ^ " in " ^ Int.toString n ^ " bits")
       else (List.tabulate (n - ln, fn _ => false)) @ bs
    end
fun signed_to_n_bits n i =
    if i < 0
    then to_n_bits n (IntInf.pow (2,n) + i)
    else false::(to_n_bits (n-1) i)
end

local
    fun f [] n = n
      | f (true::t) n = f t (2*n + 1)
      | f (false::t) n = f t (2*n)
in
fun from_bits l = f l 0
fun to_hex l =
    let val i = from_bits l
    in Int.fmt StringCvt.HEX i
    end
end


(* We keep a list of registers that have already been seen ("rs" in the code
   below so that we increase the probability that they will be reused. *)

(* Duplicates in the seen-registers-list would increase the probability of
   them being chosen again, so avoid that when updating the list. *)
fun update_rs rs [] = ()
  | update_rs rs (h::t) =
    if List.exists (fn x => h = x) (!rs)
    then update_rs rs t
    else (rs := h::(!rs); update_rs rs t)


end
end
