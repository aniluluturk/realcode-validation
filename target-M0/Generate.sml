structure Generate :> Generate = struct

open HolKernel boolLib RandGen GenerationTools

(* Specialised functions for producing particular types of value. *)

val reg3_list = List.tabulate (8, fn i => i)
val reglist_list = List.tabulate (16, fn i => i) (* Will be filtered *)
val reg4notpc_list = List.tabulate (15, fn i => i)

(* We keep a list of registers that have already been seen ("rs" in the code
   below so that we increase the probability that they will be reused. *)

fun reg3 (g,rs) =
    let val rs3 = List.filter (fn i => i < 8) (!rs)
        val r = hd (select_n g (reg3_list@rs3) 1)
        val () = update_rs rs [r]
    in to_n_bits 3 r
    end
fun reg4notpc (g,rs) =
    let val rs4 = List.filter (fn i => i < 15) (!rs)
        val r = hd (select_n g (reg4notpc_list@rs4) 1)
        val () = update_rs rs [r]
    in to_n_bits 4 r
    end

(* All zeros not allowed *)
fun reglist (g,rs) nine =
    let val size = case nine of NONE => 8 | (SOME _) => 9
        fun valid NONE i = i < 8
          | valid (SOME r) i = i < 8 orelse i = r
        val n = 1 + (genint g (size-1))
        val rsl = List.filter (valid nine) (reglist_list@(!rs))
        val l = select_n g rsl n
        val () = update_rs rs l
        fun bit NONE n = List.exists (fn r => r = 7-n) l
          | bit (SOME r) 0 = List.exists (fn r' => r' = r) l
          | bit (SOME _) n = List.exists (fn r => r = 8-n) l
    in List.tabulate (size, bit nine)
    end
(* STM only allows Rn to appear in the register list if it's the lowest reg *)
fun regliststm (g,rs) =
    let val n = 1 + (genint g 7)
        val rsl = List.filter (fn i => i<8) (reglist_list@(!rs))
        val l = select_n g rsl n
        fun bit8 n = List.exists (fn r => r = 7-n) l
        fun invert_min [] (min,acc) = min::acc
          | invert_min (h::t) (min,acc) =
            if List.exists (fn x => x = h) l
            then invert_min t (if h < min then h else min, acc)
            else invert_min t (min,h::acc)
        val rn_choices = invert_min reg3_list (hd l,[])
        val rsl' = List.filter (fn x => mem x rn_choices) (!rs)
        val rn = hd (select_n g (rn_choices@rsl') 1)
        val () = update_rs rs (rn::l)
    in (to_n_bits 3 rn) @ (List.tabulate (8, bit8))
    end
(* The odd pairing for 4-bit registers *)
fun mk_reg4pair m n = (hd n)::m@(tl n)
fun reg4notpcpair g = mk_reg4pair (reg4notpc g) (reg4notpc g)
(* CMP's T2 encoding does not permit both registers to be in R0-R7 *)
fun reg4notBoth07 (g,rs) =
    let val rs0 = !rs
        val a = reg4notpc (g,rs)
        val b = reg4notpc (g,rs)
    in if hd a orelse hd b then mk_reg4pair a b else
       (rs := rs0; reg4notBoth07 (g,rs))
    end

(* No 1110 or 1111 for conditions *)
fun cond (g,_) = to_n_bits 4 (genint g 13)


(* Small language for describing instruction formats *)

datatype instr_format =
         Lit of int list
       | Reg3
       | Reg4NotPC
       | Reg4NotPCPair
       | CmpRegs
       | RegList of int option (* Optionally including PC or LR *)
       | STMRegs
       | Cond (* Imm 4, but avoiding 1110 and 1111, which are different *)
       | Imm of int
       | BLdispl (* allow us to pick suitable values for large BLs *)

(* Instructions that we want to generate *)

(* Note that none of the 4-bit register fields include the PC; some cases are
   presented separately in the unsupported list. *)

(* Weighting: take plain ALU instructions to be 1 unit, multiply up
   branches to one per condition *)

val instrs = [
    (1,([Lit [0,1,0,0,0,0,0,1,0,1], Reg3, Reg3],           "ADC (reg)")),
    (1,([Lit [0,0,0,1,1,1,0], Imm 3, Reg3, Reg3],          "ADD (imm) T1")),
    (1,([Lit [0,0,1,1,0], Reg3, Imm 8],                    "ADC (reg) T2")),
    (1,([Lit [0,0,0,1,1,0,0], Reg3, Reg3, Reg3],           "ADD (reg) T1")),
    (1,([Lit [0,1,0,0,0,1,0,0], Reg4NotPCPair],            "ADD (reg) T2, ADD (SP+reg)")),
    (1,([Lit [0,1,0,0,0,1,0,0], Lit [1], Reg4NotPC, Lit [1,1,1]], "ADD (reg) T2 Rdn=PC")),
    (1,([Lit [1,0,1,0,1], Reg3, Imm 8],                    "ADC (SP+imm) T1")),
    (1,([Lit [1,0,1,1,0,0,0,0,0], Imm 7],                  "ADD (SP+imm) T2")),
    (1,([Lit [0,1,0,0,0,0,0,0,0,0], Reg3, Reg3],           "AND (reg)")),
    (1,([Lit [0,0,0,1,0], Imm 5, Reg3, Reg3],              "ASR (imm)")),
    (1,([Lit [0,1,0,0,0,0,0,1,0,0], Reg3, Reg3],           "ASR (reg)")),
    (14,([Lit [1,1,0,1], Cond, Imm 8],                     "B T1")),
    (1,([Lit [1,1,1,0,0], Imm 11],                         "B T2")),
    (1,([Lit [0,1,0,0,0,0,1,1,1,0], Reg3, Reg3],           "BIC (reg)")),
    (1,([Lit [1,1,1,1,0], BLdispl],                        "BL")),
    (1,([Lit [0,1,0,0,0,1,1,1,1], Reg4NotPC, Lit [0,0,0]], "BLX")),
    (1,([Lit [0,1,0,0,0,1,1,1,0], Reg4NotPC, Lit [0,0,0]], "BX")),
    (1,([Lit [0,1,0,0,0,0,1,0,1,1], Reg3, Reg3],           "CMN (reg)")),
    (1,([Lit [0,0,1,0,1], Reg3, Imm 8],                    "CMP (imm)")),
    (1,([Lit [0,1,0,0,0,0,1,0,1,0], Reg3, Reg3],           "CMP (reg) T1")),
    (1,([Lit [0,1,0,0,0,1,0,1], CmpRegs],                  "CMP (reg) T2")),
    (1,([Lit [0,1,0,0,0,0,0,0,0,1], Reg3, Reg3],           "EOR (reg)")),
    (1,([Lit [1,1,0,0,1], Reg3, RegList NONE],             "LDM, LDMIA, LDMFD")),
    (1,([Lit [0,1,1,0,1], Imm 5, Reg3, Reg3],              "LDR (imm) T1")),
    (1,([Lit [1,0,0,1,1], Reg3, Imm 8],                    "LDR (imm) T2")),
    (1,([Lit [0,1,0,0,1], Reg3, Imm 8],                    "LDR (lit)")),
    (1,([Lit [0,1,0,1,1,0,0], Reg3, Reg3, Reg3],           "LDR (reg)")),
    (1,([Lit [0,1,1,1,1], Imm 5, Reg3, Reg3],              "LDRB (imm)")),
    (1,([Lit [0,1,0,1,1,1,0], Reg3, Reg3, Reg3],           "LDRB (reg)")),
    (1,([Lit [1,0,0,0,1], Imm 5, Reg3, Reg3],              "LDRH (imm)")),
    (1,([Lit [0,1,0,1,1,0,1], Reg3, Reg3, Reg3],           "LDRH (reg)")),
    (1,([Lit [0,1,0,1,0,1,1], Reg3, Reg3, Reg3],           "LDRSB (reg)")),
    (1,([Lit [0,1,0,1,1,1,1], Reg3, Reg3, Reg3],           "LDRSH (reg)")),
    (1,([Lit [0,0,0,0,0], Imm 5, Reg3, Reg3],              "LSL (imm)")),
    (1,([Lit [0,1,0,0,0,0,0,0,1,0], Reg3, Reg3],           "LSL (reg)")),
    (1,([Lit [0,0,0,0,1], Imm 5, Reg3, Reg3],              "LSR (imm)")),
    (1,([Lit [0,1,0,0,0,0,0,0,1,1], Reg3, Reg3],           "LSR (reg)")),
    (1,([Lit [0,0,1,0,0], Reg3, Imm 8],                    "MOV (imm)")),
    (1,([Lit [0,1,0,0,0,1,1,0], Reg4NotPCPair],            "MOV (reg) T1")),
    (* This is really just LSL (imm) with 0 for the immediate
    (1,([Lit [0,0,0,0,0,0,0,0,0,0], Reg3, Reg3],           "MOV (reg) T2")), *)
    (1,([Lit [0,1,0,0,0,1,1,0], Lit [1], Reg4NotPC, Lit [1,1,1]], "MOV (reg) T1 Rd=PC")),
    (1,([Lit [0,1,0,0,0,0,1,1,0,1], Reg3, Reg3],           "MUL")),
    (1,([Lit [0,1,0,0,0,0,1,1,1,1], Reg3, Reg3],           "MVN")),
    (1,([Lit ([1,0,1,1]@[1,1,1,1]@[0,0,0,0]@[0,0,0,0])],   "NOP")),
    (1,([Lit [0,1,0,0,0,0,1,1,0,0], Reg3, Reg3],           "ORR (reg)")),
    (1,([Lit [1,0,1,1,1,1,0], RegList (SOME 15)],          "POP")),
    (1,([Lit [1,0,1,1,0,1,0], RegList (SOME 14)],          "PUSH")),
    (1,([Lit [1,0,1,1,1,0,1,0,0,0], Reg3, Reg3],           "REV")),
    (1,([Lit [1,0,1,1,1,0,1,0,0,1], Reg3, Reg3],           "REV16")),
    (1,([Lit [1,0,1,1,1,0,1,0,1,1], Reg3, Reg3],           "REVSH")),
    (1,([Lit [0,1,0,0,0,0,0,1,1,1], Reg3, Reg3],           "ROR (reg)")),
    (* NB: the imm for RSB (imm) is always zero! *)
    (1,([Lit [0,1,0,0,0,0,1,0,0,1], Reg3, Reg3],           "RSB (imm)")),
    (1,([Lit [0,1,0,0,0,0,0,1,1,0], Reg3, Reg3],           "SBC (reg)")),
    (1,([Lit [1,1,0,0,0], STMRegs],                        "STM, STMIA, STMEA")),
    (1,([Lit [0,1,1,0,0], Imm 5, Reg3, Reg3],              "STR (imm) T1")),
    (1,([Lit [1,0,0,1,0], Reg3, Imm 8],                    "STR (imm) T2")),
    (1,([Lit [0,1,0,1,0,0,0], Reg3, Reg3, Reg3],           "STR (reg)")),
    (1,([Lit [0,1,1,1,0], Imm 5, Reg3, Reg3],              "STRB (imm)")),
    (1,([Lit [0,1,0,1,0,1,0], Reg3, Reg3, Reg3],           "STRB (reg)")),
    (1,([Lit [1,0,0,0,0], Imm 5, Reg3, Reg3],              "STRH (imm)")),
    (1,([Lit [0,1,0,1,0,0,1], Reg3, Reg3, Reg3],           "STRH (reg)")),
    (1,([Lit [0,0,0,1,1,1,1], Imm 3, Reg3, Reg3],          "SUB (imm) T1")),
    (1,([Lit [0,0,1,1,1], Reg3, Imm 8],                    "SUB (imm) T2")),
    (1,([Lit [0,0,0,1,1,0,1], Reg3, Reg3, Reg3],           "SUB (reg)")),
    (1,([Lit [1,0,1,1,0,0,0,0,1], Imm 7],                  "SUB (SP-imm)")),
    (1,([Lit [1,0,1,1,0,0,1,0,0,1], Reg3, Reg3],           "SXTB")),
    (1,([Lit [1,0,1,1,0,0,1,0,0,0], Reg3, Reg3],           "SXTH")),
    (1,([Lit [0,1,0,0,0,0,1,0,0,0], Reg3, Reg3],           "TST")),
    (1,([Lit [1,0,1,1,0,0,1,0,1,1], Reg3, Reg3],           "UXTB")),
    (1,([Lit [1,0,1,1,0,0,1,0,1,0], Reg3, Reg3],           "UXTH"))
]

(* Instructions that are reasonable but that the model doesn't currently support *)

val unsupported_instrs = [
    (* Unsupported forms of ADD and MOV with the PC *)
    (* NB: we cheat a little and use Imm instead of a specialised register generator *)
    (* If I were to use these patterns in anger, I ought to prevent Rd being the PC,
       which is UNPREDICTABLE *)
    (1,([Lit [0,1,0,0,0,1,0,0], Imm 1, Lit [1,1,1,1], Imm 3], "ADD (reg) T2 Rm=PC")),
    (1,([Lit [0,1,0,0,0,1,1,0], Imm 1, Lit [1,1,1,1], Imm 3], "MOV (reg) T1 Rm=PC")),

    (1,([Lit [1,0,1,0,0], Reg3, Imm 8],                    "ADR")),

    (* Barriers and hints *)
    (1,([Lit ([1,1,1,1,0]@[0]@[1,1,1]@[0,1]@[1]@[1,1,1,1]@[1,0]@[0]@[0]@[1,1,1,1]@[0,1,0,1]
     @[1,1,1,1])], (* These last four bits are the option, in principle the
                      behaviour for other options is fixed for (say) M0, but I'm
                      not keen on testing that. *)
                                                        "DMB")),
    (1,([Lit ([1,1,1,1,0]@[0]@[1,1,1]@[0,1]@[1]@[1,1,1,1]@[1,0]@[0]@[0]@[1,1,1,1]@[0,1,0,0]
     @[1,1,1,1])], (* Ditto *)
                                                        "DSB")),
    (1,([Lit ([1,1,1,1,0]@[0]@[1,1,1]@[0,1]@[1]@[1,1,1,1]@[1,0]@[0]@[0]@[1,1,1,1]@[0,1,1,0]
     @[1,1,1,1])], (* Ditto *)
                                                        "ISB")),
    (1,([Lit ([1,0,1,1]@[1,1,1,1]@[0,1,0,0]@[0,0,0,0])],   "SEV")),
    (1,([Lit ([1,0,1,1]@[1,1,1,1]@[0,0,0,1]@[0,0,0,0])],   "YIELD"))
]

(* Instructions that can't be easily tested in our setting. *)

val difficult_instrs = [
    (1,([Lit [1,0,1,1,1,1,1,0], Imm 8],                    "BKPT")),
    (1,([Lit [1,1,0,1,1,1,1,1], Imm 8],                    "SVC")),
    (1,([Lit [1,1,0,1,1,1,1,0], Imm 8],                    "UDF T1")),
    (1,([Lit ([1,1,1]@[1,0]@[1,1,1,1,1,1,1]), Imm 4, Lit [1,0,1,0], Imm 12],
                                                           "UDF T2")),
    (1,([Lit ([1,0,1,1]@[1,1,1,1]@[0,0,1,0]@[0,0,0,0])],   "WFE")),
    (1,([Lit ([1,0,1,1]@[1,1,1,1]@[0,0,1,1]@[0,0,0,0])],   "WFI"))
]

(* TODO: A6-158 says that MRS and MSR (reg) have some application uses,
   should maybe move those above?
*)

val system_instrs = [
    (1,([Lit ([1,0,1,1]@[0,1,1,0]@[0,1,1]), Imm 1, Lit [0,0,1,0]], "CPS")),
    (* Technically, SYSm is more constrained than Imm 8, and the register shouldn't be SP *)
    (1,([Lit ([1,1,1,1,0]@[0]@[1,1,1,1]@[1]@[0]@[1,1,1,1]@[1,0]@[0]@[0]), Reg4NotPC, Imm 8], "MRS")),
    (1,([Lit ([1,1,1,1,0]@[0]@[1,1,1,0]@[0]@[0]), Reg4NotPC, Lit [1,0,0,0,1,0,0,0], Imm 8], "MSR (reg)"))
]

val all_instrs = instrs @ unsupported_instrs @ difficult_instrs @ system_instrs

(* Generation of instructions from formats *)

fun instr_gen g fmt =
    let fun f (Lit l)         = tbl l
          | f (Reg3)          = reg3 g
          | f (Reg4NotPC)     = reg4notpc g
          | f (Reg4NotPCPair) = reg4notpcpair g
          | f (CmpRegs)       = reg4notBoth07 g
          | f (RegList pclr)  = reglist g pclr
          | f (STMRegs)       = regliststm g
          | f (Cond)          = cond g
          | f (Imm n)         = bits (fst g) n
          | f (BLdispl)       =
            (* off is in *half-words* *)
            let val off = (genint (fst g) Target.sram_size) - (Target.sram_size div 2)
                val bs = signed_to_n_bits 24 off
                val (s::i1::i2::t) = bs
                val (j1,j2) = (eor (not i1) s, eor (not i2) s)
                val imm10 = List.take (t,10)
                val imm11 = List.drop (t,10)
            in s::(imm10 @ [true,true,j1,true,j2] @ imm11)
            end
    in List.concat (map f fmt)
    end

fun mk_instrs_gen is = map (fn (n,i) => (n,(fn g => instr_gen g (fst i), snd i))) is

val instrs_gen = mk_instrs_gen instrs
val other_instrs_gen = mk_instrs_gen (unsupported_instrs @ difficult_instrs @ system_instrs)

local 
fun pick g rs _ =
    let val (f,s) = snd (weighted_select instrs_gen g)
    in (f (g,rs), s)
    end
in
fun pick_one_instr g = pick g (ref []) 0
fun pick_instrs g n =
    let val rs = ref []
    in List.tabulate (n, pick g rs)
    end
end

(* Sample one instruction from each format, except for conditionals where we
   produce a sample for each conditional. *)
local
fun b false = 0
  | b true  = 1

fun expand_cond [] = [[]]
  | expand_cond (Cond::t) =
    let val ts = expand_cond t
        val ll = List.tabulate (14, fn i => map (cons (Lit (map b (to_n_bits 4 i)))) ts)
    in List.concat ll
    end
  | expand_cond (h::t) = map (cons h) (expand_cond t)
in
fun sample_instrs g =
    let val fmts = List.concat (map (fn (_,(fmt,name)) => map (fn x => (x,name))
                                    (expand_cond fmt)) instrs)
    in map (fn (f,n) => (to_hex (instr_gen (g,ref []) f), n)) fmts
    end
end

(* Length of instructions, as a sanity check *)

local
    fun len (Lit l)         = length l
      | len (Reg3)          = 3
      | len (Reg4NotPC)     = 4
      | len (Reg4NotPCPair) = 8
      | len (CmpRegs)       = 8
      | len (RegList pclr)  = (case pclr of (SOME _) => 9 | NONE => 8)
      | len (STMRegs)       = 3 + 8
      | len (Cond)          = 4
      | len (Imm n)         = n
      | len (BLdispl)       = 1+10+2+1+1+1+11
in
val instr_len = List.foldl (fn (x,n) => n + len x) 0
end

(* Make sure everything is 16 or 32 bits long; will implicitly check that Var
   formats are well-formed. *)
fun check_instrs_len is = 
    List.mapPartial (fn (_,(fmt,name)) =>
            let val n = instr_len fmt in
                if n <> 16 andalso n <> 32
                then SOME (name ^ " has bad length " ^ Int.toString n)
                else NONE
            end) is

(* Try the instruction generators in [is_gen] against m0_progLib's spec
   generator, up to [n] times each. *)
fun try_instrs is_gen n =
    let val r = newgen()
        fun tryf (f,s) 0 = NONE
          | tryf (f,s) n =
            let val i = f (r, ref [])
                val h = to_hex i
            in (m0_progLib.m0_spec_hex h;
                if length i = 16 orelse length i = 32
                then tryf (f,s) (n-1)
                else SOME (h ^ " " ^ s))
               handle e => SOME (h ^ " " ^ s)
            end
    in List.mapPartial (fn (_,fs) => tryf fs n) is_gen
    end

(* Find example of instructions with more than one spec; ought to be just branches *)
fun find_multiple_instrs is_gen n =
    let val r = newgen()
        fun tryf (f,s) 0 = NONE
          | tryf (f,s) n =
            let val i = f (r, ref [])
                val h = to_hex i
            in let val ss = m0_progLib.m0_spec_hex h in
                   if length ss > 1 then SOME (s,h,ss) else tryf (f,s) (n-1)
               end
               handle e => tryf (f,s) (n-1)
            end
    in List.mapPartial (fn (_,fs) => tryf fs n) is_gen
    end
(* filter (fn (_,_,[a,b]) => dest_thm a <> dest_thm b) dbls; *)

(* Test various aspects of the instruction formats against the model.  The
   argument is the maximum number of times to try each individual format. *)

fun test_instrs n =
    let val bad = check_instrs_len all_instrs
        val lmsg = case bad of
                    [] => "Format lengths OK\n"
                  | _ => "Bad format lengths: " ^ String.concatWith "\n" bad ^ "\n"
        val bad = try_instrs instrs_gen n
        val gmsg = case bad of
                    [] => "Generation OK.\n"
                  | _ => "Trouble generating " ^
                         String.concatWith ", " bad ^"\n"
        val good = try_instrs other_instrs_gen n
        val xmsg = if length good < length other_instrs_gen
                then "Extra specs found\n"
                else "No extra specs found\n"
        val multi = find_multiple_instrs instrs_gen n
            (* there should be at most one entry in multi (for branches) *)
        val mmsg = if length multi > 1 then "Extra multiple specs found\n"
                   else "No extra multiple specs found\n"
    in app print ["\n",lmsg,gmsg,xmsg,mmsg]
    end
    

fun gen_one_instr gen =
    let val (i,s) = pick_one_instr gen
    in (to_hex i, s, length i div 8)
    end
    handle e => raise wrap_exn "Generate" "gen_one_instr" e

fun gen_instrs gen n =
    let val is = pick_instrs gen n
    in map (fn (i,s) => (to_hex i, s, length i div 8)) is
    end
    handle e => raise wrap_exn "Generate" "gen_instrs" e


(* Code for extracting a list of registers *chosen* during generation.
   This is for getting some idea of how well the register biasing is
   working, and is not necessary for generation. *)

local
    fun apo f (SOME x) = SOME (f x)
      | apo f NONE = NONE


    fun strip bs [] = SOME bs
      | strip [] _  = NONE
      | strip (1::t) (1::t') = strip t t'
      | strip (0::t) (0::t') = strip t t'
      | strip (_::t) (_::t') = NONE

    fun rl [] [] = []
      | rl (1::bs) (r::rs) = r::(rl bs rs)
      | rl (0::bs) (_::rs) =     rl bs rs
      | rl _ _ = raise Empty

    fun from_i 0 = false
      | from_i 1 = true
      | from_i _ = failwith "Integer booleans should be 0/1"

    fun from_ibits is = from_bits (map from_i is)

    fun to_i false = 0
      | to_i true = 1

    fun to_ibits n i = map to_i (to_n_bits n i)

    fun hex_to_bits (h,_,s) =
        let val n = 8*s
            val i = Option.valOf (StringCvt.scanString (Int.scan StringCvt.HEX) h)
        in to_ibits n i
        end

    fun mapPartialFmt f [] = []
      | mapPartialFmt f ((_,(h,txt))::t) =
        case f h of
            NONE => mapPartialFmt f t
          | SOME h' => (h',txt)::(mapPartialFmt f t)
in
fun match_frag [] _ = raise Empty
  | match_frag bs (Lit bs') = apo (fn x => ([],x)) (strip bs bs')
  | match_frag (b0::b1::b2::t) Reg3 = SOME ([from_ibits [b0,b1,b2]], t)
  | match_frag _ Reg3 = NONE
  | match_frag (1::1::1::1::t) Reg4NotPC = NONE (* PC *)
  | match_frag (b0::b1::b2::b3::t) Reg4NotPC = SOME ([from_ibits [b0,b1,b2,b3]], t)
  | match_frag _ Reg4NotPC = NONE
  | match_frag (1::_::_::_::_::1::1::1::t) Reg4NotPCPair = NONE
  | match_frag (_::1::1::1::1::t) Reg4NotPCPair = NONE
  | match_frag (b0::c0::c1::c2::c3::b1::b2::b3::t) Reg4NotPCPair =
    SOME ([from_ibits [b0,b1,b2,b3], from_ibits [c0,c1,c2,c3]], t)
  | match_frag _ Reg4NotPCPair = NONE
  | match_frag (1::_::_::_::_::1::1::1::t) CmpRegs = NONE
  | match_frag (_::1::1::1::1::t) CmpRegs = NONE
  | match_frag (1::c0::c1::c2::c3::b1::b2::b3::t) CmpRegs =
    SOME ([from_ibits [ 1,b1,b2,b3], from_ibits [c0,c1,c2,c3]], t)
  | match_frag (b0::1::c1::c2::c3::b1::b2::b3::t) CmpRegs =
    SOME ([from_ibits [b0,b1,b2,b3], from_ibits [ 1,c1,c2,c3]], t)
  | match_frag _ CmpRegs = NONE
  | match_frag (r7::r6::r5::r4::r3::r2::r1::r0::t) (RegList NONE) =
    SOME (rl [r0,r1,r2,r3,r4,r5,r6,r7] [0,1,2,3,4,5,6,7], t)
  | match_frag (rx::r7::r6::r5::r4::r3::r2::r1::r0::t) (RegList (SOME r)) =
    SOME (rl [r0,r1,r2,r3,r4,r5,r6,r7,rx] [0,1,2,3,4,5,6,7,r], t)
  | match_frag _ (RegList _) = NONE
  (* Note: doesn't enforce extra condition in regliststm *)
  | match_frag (b0::b1::b2::r7::r6::r5::r4::r3::r2::r1::r0::t) STMRegs =
    SOME ((from_ibits [b0,b1,b2])::(rl [r0,r1,r2,r3,r4,r5,r6,r7] [0,1,2,3,4,5,6,7]), t)
  | match_frag _ STMRegs = NONE
  | match_frag (1::1::1::0::_) Cond = NONE
  | match_frag (1::1::1::1::_) Cond = NONE
  | match_frag (_::_::_::_::t) Cond = SOME ([], t)
  | match_frag _ Cond = NONE
  | match_frag bs (Imm n) = if length bs >= n then SOME ([], List.drop (bs,n)) else NONE
  | match_frag bs BLdispl = if length bs >= 1+10+2+1+1+1+11 then SOME ([], List.drop (bs,1+10+2+1+1+1+11)) else NONE

fun match_fmt [] [] = SOME []
  | match_fmt bs [] = NONE
  | match_fmt bs (frag::t) =
    (case match_frag bs frag of
         NONE => NONE
       | SOME (rs,bs') => case match_fmt bs' t of NONE => NONE | SOME rs' => SOME (rs@rs'))

fun match_fmts fmts h =
    let val bs = hex_to_bits h
    in case mapPartialFmt (match_fmt bs) fmts of
           [] => failwith ("No instruction format matches " ^ (#1 h))
         | [(rs,_)] => rs
         | _ => failwith ("Multiple instruction formats match " ^ (#1 h))
    end

fun regs_chosen_in_instr h =
    match_fmts instrs h
end

end
