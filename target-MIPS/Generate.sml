structure Generate : sig
             include Generate
             val find_bad_instr : string -> bool list
             val respect_delay_slots : bool ref
          end = struct

open GenerationTools RandGen

datatype instr_frag =
   Lit of int list
 | Imm of int
 | Reg
 (* Not zero register *)
 | RegNZ
 (* Not link register, 31 *)
 | RegNL
 (* Offset in bytes from a given register *)
 | RegOffset
 (* Offset in words from the PC (actually, probably the address of the delay slot) *)
 | PCOffset
 | JumpTarget

val reg_list = List.tabulate (32, fn i => i)

fun mk_reg (g,rs) =
    let val selection = reg_list@ !rs
        val r = hd (select_n g selection 1)
        val () = update_rs rs [r]
    in to_n_bits 5 r
    end
fun mk_regnz (g,rs) =
    let val selection = List.filter (fn i => i > 0) (reg_list@ !rs)
        val r = hd (select_n g selection 1)
        val () = update_rs rs [r]
    in to_n_bits 5 r
    end
fun mk_regnl (g,rs) =
    let val selection = List.filter (fn i => i < 31) (reg_list@ !rs)
        val r = hd (select_n g selection 1)
        val () = update_rs rs [r]
    in to_n_bits 5 r
    end

fun mk_offset g dv =
    let val max_magnitude = Target.sram_size div dv
        val off = genint g (2*max_magnitude) - max_magnitude
    in signed_to_n_bits 16 off
    end

(* Jump targets are only a 26-bit subsequence of the bits of the
   target address. *)

val jump_low = (Target.sram_start mod (IntInf.pow (2,28))) div 4

fun mk_jump_target g =
    let val pos = genint g (Target.sram_size div 4)
    in to_n_bits 26 (jump_low + pos)
    end

(* TODO: we could also allow larger values for RegOffset, hoping that the
   SMT solver will choose appropriate base registers, but I still need to
   think about that. *)

fun mk_frag (g,rs) (Lit l)    = tbl l
  | mk_frag (g,rs) (Imm n)    = bits g n
  | mk_frag (g,rs) Reg        = mk_reg (g,rs)
  | mk_frag (g,rs) RegNZ      = mk_regnz (g,rs)
  | mk_frag (g,rs) RegNL      = mk_regnl (g,rs)
  | mk_frag (g,rs) RegOffset  = mk_offset g 1
  | mk_frag (g,rs) PCOffset   = mk_offset g 4
  | mk_frag (g,rs) JumpTarget = mk_jump_target g

fun frag_size (Lit l)    = length l
  | frag_size (Imm n)    = n
  | frag_size Reg        = 5
  | frag_size RegNZ      = 5
  | frag_size RegNL      = 5
  | frag_size RegOffset  = 16
  | frag_size PCOffset   = 16
  | frag_size JumpTarget = 26

fun fmt_size (fmt,s) = (foldl (op +) 0 (map frag_size fmt), s)

fun mk_instr g fmt = List.concat (map (mk_frag g) fmt)

(* This is based on the decoder from the simulator version of the L3 model, by
   cut, paste, replace and partitioning.

   NB: a lot of the RegNZ are places where I think the processor does have a
   defined instruction, but we don't get a step theorem for it. *)

val normal_instrs : (instr_frag list * string) list =
     [([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [0,0,0,0,0,0]], "SLL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [0,0,0,0,1,0]], "SRL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [0,0,0,0,1,1]], "SRA"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,0,1,0,0]], "SLLV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,0,1,1,0]], "SRLV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,0,1,1,1]], "SRAV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,1,0,1,0]], "MOVZ"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,1,0,1,1]], "MOVN"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], Reg, Lit [0,0,0,0,0,0,1,0,0,0,0]], "MFHI"),
      ([Lit [0,0,0,0,0,0], Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1]], "MTHI"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], Reg, Lit [0,0,0,0,0,0,1,0,0,1,0]], "MFLO"),
      ([Lit [0,0,0,0,0,0], Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1]], "MTLO"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,1,0,1,0,0]], "DSLLV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,1,0,1,1,0]], "DSRLV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,1,0,1,1,1]], "DSRAV"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0]], "MULT"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1]], "MULTU"),
      ([Lit [0,0,0,0,0,0], Reg, RegNZ, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0]], "DIV"),
      ([Lit [0,0,0,0,0,0], Reg, RegNZ, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1]], "DIVU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0]], "DMULT"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1]], "DMULTU"),
      ([Lit [0,0,0,0,0,0], Reg, RegNZ, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0]], "DDIV"),
      ([Lit [0,0,0,0,0,0], Reg, RegNZ, Lit [0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1]], "DDIVU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,0,0,0]], "ADD"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,0,0,1]], "ADDU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,0,1,0]], "SUB"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,0,1,1]], "SUBU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,1,0,0]], "AND"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,1,0,1]], "OR"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,1,1,0]], "XOR"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,0,1,1,1]], "NOR"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,0,1,0]], "SLT"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,0,1,1]], "SLTU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,1,0,0]], "DADD"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,1,0,1]], "DADDU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,1,1,0]], "DSUB"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,1,0,1,1,1,1]], "DSUBU"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,0,0,0]], "DSLL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,0,1,0]], "DSRL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,0,1,1]], "DSRA"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,1,0,0]], "DSLL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,1,1,0]], "DSRL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Reg, Reg, Imm 5, Lit [1,1,1,1,1,1]], "DSRA"),
      ([Lit [0,0,1,1,1,1,0,0,0,0,0], Reg, Imm 16], "LUI"),
      ([Lit [0,1,1,1,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], "MADD"),
      ([Lit [0,1,1,1,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]], "MADDU"),
      ([Lit [0,1,1,1,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0]], "MSUB"),
      ([Lit [0,1,1,1,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1]], "MSUBU"),
      ([Lit [0,1,1,1,0,0], Reg, Reg, Reg, Lit [0,0,0,0,0,0,0,0,0,1,0]], "MUL"),
      ([Lit [0,0,1,0,0,0], Reg, Reg, Imm 16], "ADDI"),
      ([Lit [0,0,1,0,0,1], Reg, Reg, Imm 16], "ADDIU"),
      ([Lit [0,0,1,0,1,0], Reg, Reg, Imm 16], "SLTI"),
      ([Lit [0,0,1,0,1,1], Reg, Reg, Imm 16], "SLTIU"),
      ([Lit [0,0,1,1,0,0], Reg, Reg, Imm 16], "ANDI"),
      ([Lit [0,0,1,1,0,1], Reg, Reg, Imm 16], "ORI"),
      ([Lit [0,0,1,1,1,0], Reg, Reg, Imm 16], "XORI"),
      ([Lit [0,1,1,0,0,0], Reg, Reg, Imm 16], "DADDI"),
      ([Lit [0,1,1,0,0,1], Reg, Reg, Imm 16], "DADDIU"),
      ([Lit [1,0,0,0,0,0], Reg, RegNZ, RegOffset], "LB"),
      ([Lit [1,0,0,0,0,1], Reg, RegNZ, RegOffset], "LH"),
      ([Lit [1,0,0,0,1,1], Reg, RegNZ, RegOffset], "LW"),
      ([Lit [1,0,0,1,0,0], Reg, RegNZ, RegOffset], "LBU"),
      ([Lit [1,0,0,1,0,1], Reg, RegNZ, RegOffset], "LHU"),
      ([Lit [1,0,0,1,1,1], Reg, RegNZ, RegOffset], "LWU"),
      ([Lit [1,0,1,0,0,0], Reg, RegNZ, RegOffset], "SB"),
      ([Lit [1,0,1,0,0,1], Reg, RegNZ, RegOffset], "SH"),
      ([Lit [1,0,1,0,1,1], Reg, RegNZ, RegOffset], "SW"),
      ([Lit [1,1,0,0,0,0], Reg, RegNZ, RegOffset], "LL"),
      ([Lit [1,1,0,1,0,0], Reg, RegNZ, RegOffset], "LLD"),
      ([Lit [1,1,0,1,1,1], Reg, RegNZ, RegOffset], "LD"),
      ([Lit [1,1,1,1,1,1], Reg, RegNZ, RegOffset], "SD")]

(* Instructions that should not occur in a delay slot *)
val non_delay_slot_instrs : (instr_frag list * string) list =
     [([Lit [0,0,0,0,0,0], Reg, Lit [0,0,0,0,0,0,0,0,0,0], Imm 5, Lit [0,0,1,0,0,0]], "JR"),
      ([Lit [0,0,0,0,0,0], Reg, Lit [0,0,0,0,0], Reg, Imm 5, Lit [0,0,1,0,0,1]], "JALR"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,0,0,0,0], PCOffset], "BLTZ"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,0,0,0,1], PCOffset], "BGEZ"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,0,0,1,0], PCOffset], "BLTZL"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,0,0,1,1], PCOffset], "BGEZL"),
      ([Lit [0,0,0,0,0,1], RegNL, Lit [1,0,0,0,0], PCOffset], "BLTZAL"),
      ([Lit [0,0,0,0,0,1], RegNL, Lit [1,0,0,0,1], PCOffset], "BGEZAL"),
      ([Lit [0,0,0,0,0,1], RegNL, Lit [1,0,0,1,0], PCOffset], "BLTZALL"),
      ([Lit [0,0,0,0,0,1], RegNL, Lit [1,0,0,1,1], PCOffset], "BGEZALL"),
      ([Lit [0,0,0,0,1,0], JumpTarget], "J"),
      ([Lit [0,0,0,0,1,1], JumpTarget], "JAL"),
      ([Lit [0,0,0,1,0,0], Reg, Reg, PCOffset], "BEQ"),
      ([Lit [0,0,0,1,0,1], Reg, Reg, PCOffset], "BNE"),
      ([Lit [0,0,0,1,1,0], Reg, Lit [0,0,0,0,0], PCOffset], "BLEZ"),
      ([Lit [0,0,0,1,1,1], Reg, Lit [0,0,0,0,0], PCOffset], "BGTZ"),
      ([Lit [0,1,0,1,0,0], Reg, Reg, PCOffset], "BEQL"),
      ([Lit [0,1,0,1,0,1], Reg, Reg, PCOffset], "BNEL"),
      ([Lit [0,1,0,1,1,0], Reg, Lit [0,0,0,0,0], PCOffset], "BLEZL"),
      ([Lit [0,1,0,1,1,1], Reg, Lit [0,0,0,0,0], PCOffset], "BGTZL"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0]], "ERET")]

val unsupported_instrs : (instr_frag list * string) list =
     [([Lit [0,0,0,0,0,0,0,0,0,0,0], Imm 15, Lit [0,0,1,1,0,0]], "SYSCALL"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0], Imm 15, Lit [0,0,1,1,0,1]], "BREAK"),
      ([Lit [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], Imm 5, Lit [0,0,1,1,1,1]], "SYNC"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,0,0,0]], "TGE"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,0,0,1]], "TGEU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,0,1,0]], "TLT"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,0,1,1]], "TLTU"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,1,0,0]], "TEQ"),
      ([Lit [0,0,0,0,0,0], Reg, Reg, Imm 10, Lit [1,1,0,1,1,0]], "TNE"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,0,0,0], Imm 16], "TGEI"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,0,0,1], Imm 16], "TGEIU"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,0,1,0], Imm 16], "TLTI"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,0,1,1], Imm 16], "TLTIU"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,1,0,0], Imm 16], "TEQI"),
      ([Lit [0,0,0,0,0,1], Reg, Lit [0,1,1,1,0], Imm 16], "TNEI"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]], "TLBR"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]], "TLBWI"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0]], "TLBWR"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]], "TLBP"),
      ([Lit [0,1,0,0,0,0,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0], Imm 3], "MFC"),
      ([Lit [0,1,0,0,0,0,0,0,0,0,1], Reg, Reg, Lit [0,0,0,0,0,0,0,0], Imm 3], "DMFC"),
      ([Lit [0,1,0,0,0,0,0,0,1,0,0], Reg, Reg, Lit [0,0,0,0,0,0,0,0], Imm 3], "MTC"),
      ([Lit [0,1,0,0,0,0,0,0,1,0,1], Reg, Reg, Lit [0,0,0,0,0,0,0,0], Imm 3], "DMTC"),
      ([Lit [0,1,1,0,1,0], Reg, Reg, RegOffset], "LDL"),
      ([Lit [0,1,1,0,1,1], Reg, Reg, RegOffset], "LDR"),
      ([Lit [1,0,0,0,1,0], Reg, Reg, RegOffset], "LWL"),
      ([Lit [1,0,0,1,1,0], Reg, Reg, RegOffset], "LWR"),
      ([Lit [1,0,1,0,1,0], Reg, Reg, RegOffset], "SWL"),
      ([Lit [1,0,1,1,0,0], Reg, Reg, RegOffset], "SDL"),
      ([Lit [1,0,1,1,0,1], Reg, Reg, RegOffset], "SDR"),
      ([Lit [1,0,1,1,1,0], Reg, Reg, RegOffset], "SWR"),
      ([Lit [1,1,1,0,0,0], Reg, Reg, RegOffset], "SC"),
      ([Lit [1,1,1,1,0,0], Reg, Reg, RegOffset], "SCD"),
      ([Lit [1,0,1,1,1,1], Imm 5, Imm 5, RegOffset], "CACHE"),
      ([Lit [0,1,1,1,1,1,0,0,0,0,0], Reg, Reg, Lit [0,0,0,0,0,1,1,1,0,1,1]], "REGHWR"),
      ([Lit [0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]], "WAIT")]

val all_instrs = normal_instrs @ non_delay_slot_instrs @ unsupported_instrs

fun descl [] = "<none>"
  | descl l = String.concatWith "\n  " l

fun find_bad_instr s =
    let val (fmt,_) = case List.find (fn (_,s') => s' = s) all_instrs of
                          NONE => failwith ("Unable to find format for " ^ s)
                        | SOME x => x
        val gen = RandGen.newgen ()
        fun try () =
            let val i = mk_instr (gen, ref []) fmt
            in let val _ = Target.step (to_hex i)
               in try ()
               end handle _ => i
            end
    in try ()
    end

  fun test_instrs n =
      let val sizes = map fmt_size (non_delay_slot_instrs@normal_instrs)
          val bad = List.mapPartial (fn (32,_) => NONE | (_,s) => SOME s) sizes
          val gen = RandGen.newgen ()
          fun try f 0 fmt = (NONE,0)
            | try f n fmt =
              let val i = mk_instr (gen, ref []) fmt
                  val h = to_hex i
                  val (eg,m) = try f (n-1) fmt
                  val count = (SOME h,m+1)
                  val ignore = (eg,m)
              in if f h then ignore else count
              end
          fun tryn f (fmt,s) =
              let val (eg,bad) = try f n fmt
              in if bad = 0 then NONE else
                 SOME (s ^ " x" ^ Int.toString bad ^ " (e.g. " ^ Option.valOf eg ^ ")")
              end
          fun check_number n h = (length (Target.step h) = n) handle _ => true
          fun roundtrip h =
              let val bits = Option.valOf (BitsN.fromHexString (h,32))
              in bits = mips.Encode (mips.Decode bits)
              end
          fun stringroundtrip h =
              let val instr = mips.Decode (Option.valOf (BitsN.fromHexString (h,32)))
                  val s = mips.instructionToString instr
              in mips.OK instr = mips.instructionFromString s
              end
          val bad_unsupported = List.mapPartial (tryn (Lib.can Target.step)) (non_delay_slot_instrs @ normal_instrs)
          val oops_supported = List.mapPartial (tryn (not o Lib.can Target.step)) unsupported_instrs
          val wrong_number3 = List.mapPartial (tryn (check_number 3)) normal_instrs
          val wrong_number1 = List.mapPartial (tryn (check_number 1)) non_delay_slot_instrs
          val roundtrip = List.mapPartial (tryn roundtrip) all_instrs
          val stringtrip = List.mapPartial (tryn stringroundtrip) all_instrs
      in print ("Bad instruction length:\n  " ^ descl bad ^ "\n" ^
                "Unsupported instructions:\n  " ^ descl bad_unsupported ^ "\n" ^
                "Unexpectedly supported instructions:\n  " ^ descl oops_supported ^ "\n" ^
                "Unexpected number of theorems:\n  " ^ descl wrong_number3 ^ "\n  "
                ^ descl wrong_number1 ^ "\n" ^
                "Fails decode / encode round trip:\n  " ^ descl roundtrip ^ "\n" ^
                "Fails decode / ToString / FromString round trip:\n  " ^ descl stringtrip ^ "\n")
      end

  fun pick_one_instr (gen,rs) delay_slot =
      let val instrs = if delay_slot then normal_instrs else non_delay_slot_instrs@normal_instrs
          val (_,(fmt,s)) = RandGen.select instrs gen
          val i = mk_instr (gen,rs) fmt
      in (to_hex i,s,4)
      end
  fun gen_one_instr gen = pick_one_instr (gen, ref []) false

  val respect_delay_slots = ref true

  (* This doesn't include the branch likely instructions, where the next
     instruction executed might or might *not* be in a delay slot; we leave
     it to the model to reject cases where it is and we put control flow in *)
  val delay_slot_must_follow = ["JR", "JALR", "BLTZ", "BGEZ", "BLTZAL", "BGEZAL", "J", "JAL", "BEQ", "BNE", "BLEZ", "BGTZ"]

  fun gen_instrs gen n =
      let val rs = ref []
          fun mk 0 _ acc = rev acc
            | mk n delayslot acc =
              let val (h,s,sz) = pick_one_instr (gen,rs) (!respect_delay_slots andalso delayslot)
                  val delayslot' = List.exists (fn s' => s = s') delay_slot_must_follow
              in mk (n-1) delayslot' ((h,s,sz)::acc)
              end
      in mk n false []
      end
  fun sample_instrs gen =
      map (fn (fmt,s) => (to_hex (mk_instr (gen, ref []) fmt),s)) (non_delay_slot_instrs@normal_instrs)
  fun regs_chosen_in_instr _ = Feedback.fail ()
end
