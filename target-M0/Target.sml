structure Target : sig
  include Target
  (* Optional additional constraint; defaults to true *)
  val align_sp_flag : bool ref
  val xmc_reset : bool
end =
struct

local
open HolKernel Parse boolLib bossLib m0_stepTheory

in

(* STMF0 Discovery *) (*
val sram_start = 0x20000000
val sram_size = 8192
val xmc_reset = false
*)

(*(* LPCxpresso 11u14 *)
val sram_start = 0x10000000
val sram_size = 4096
val xmc_reset = false
*)
(* System clock divider register (SYSAHBCLKDIV, address 0x4004 8078) 
   (doesn't seem to affect anything - shouldn't because clock on reset will
    be 12MHz)

val init = [(0x40048078, 4)]
*)

(* Flash memory access latency (doesn't appear to have side effects on SRAM)
val init = [(0x4003C010, 0, 3)]
*)

(* XMC1100 *)
val sram_start = 0x20000000
val sram_size = 16 * 1024
val xmc_reset = true
(**)


(* Cypress PSoC 41xx *)(*
val sram_start = 0x20000000
val sram_size = 4 * 1024
*)

(*
For one of these with the default Flash image, also change HWTest.reset to

fun reset debug =
    (OpenOCD.send_quiet_command debug "halt";
     OpenOCD.send_quiet_command debug "bp 0x10 2 hw";
     OpenOCD.send_quiet_command debug "reset";
     (* Hopefully long enough for openocd to reconnect *)
     OS.Process.sleep (Time.fromReal 0.5);
     OpenOCD.send_quiet_command debug "rbp 0x10")

because I couldn't get reset to work properly.  Note that 0x10 is the first
instruction of the reset handler, whose address is at location 4.  If a
different image is flashed on to the chip replace the 0x10s with the word
at location 4.
*)

val init = []


(* Ensure that we can evaluate comparisons of register names without using
   everything in cbv_CONV. *)
val () = utilsLib.add_datatypes [``:RName``] computeLib.the_compset;

val nextstate = ``NextStateM0``
fun pc s = ``^s.REG RName_PC``
val memory_field_update = ``m0_state_MEM_fupd``
val memory_type = ``:word32 -> word8``
val sort_addresses = updateLib.SORT_WORD_UPDATES_CONV ``:32``
val sort_registers = updateLib.SORT_ENUM_UPDATES_CONV ``:RName``
val state_type = ``:m0_state``

(* Target specific manipulations to make to step theorems *)
fun fixup_th th =
    th |>
    DISCH_ALL |>
    (* I've seen a few instructions such as
         c049 - stmia   r0!, {r0, r3, r6}
       leave a hypothesis involving LowestSetBit in, despite having
       concrete values everywhere.  I think m0_stepLib is supposed
       to deal with it, but we can do it here instead. *)
    REWRITE_RULE [m0Theory.LowestSetBit_def,m0_stepTheory.CountLeadingZeroBits8] |>
    UNDISCH_ALL

(* StepSMT *)
val word_size = 32
val word_type = ``:word32``
val basic_flags = ``<| N := flagN; Z := flagZ; C := flagC; V := flagV;
                       ExceptionNumber := 0w; T := T |>``
val regs_type = ``:RName->word32``

val align_sp_flag = ref true

fun describe_additional () = "align_ap:" ^ (if !align_sp_flag then "on" else "off")

(* The ARMv6-M architecture requires the bottom two bits of the stack pointers
   to be zero, but this isn't reflected in the model. *)
fun sp_check r =
  let val r = Tools.cbv_eval ``(reg :RName -> word32) ^r``
  in ``(1 >< 0) ^r = 0w : word2``
  end

fun additional_constraints th cbv_eval =
  let val (hy,cn) = dest_thm th
      fun uses r = List.exists (can (find_term (equal r))) (cn::hy)
      (* Only constrain the stack pointers if we really need to, so that
           we prefer random state over SMT-solver chosen state *)
  in
      if !align_sp_flag
      then
          map sp_check
              (List.filter uses [``RName_SP_main``, ``RName_SP_process``])
      else []
  end


(* State *)
val addr_size = word_size (* TODO: check for bad uses of word_size *)

(* TODO: can we ensure the number of constructors is correct? *)
fun rand_regs gen =
    (* The stack pointer registers must be aligned (v6m ref manual, A5.1.3) *)
    let fun rr 13 = Tools.random_word gen 30 2
          | rr 14 = Tools.random_word gen 30 2
          | rr _  = Tools.random_word gen 32 0
        val vals = List.tabulate (17, rr)
        val hol_vals = listSyntax.mk_list (vals, ``:word32``)
    in ``\r. EL (RName2num r) ^hol_vals``
    end

fun fill_in_state gen s mem =
    let val bits = RandGen.bits gen 4
        fun f [N,Z,C,V] = (N,Z,C,V)
          | f _ = fail ()
        val (N,Z,C,V) = f (map bitstringSyntax.term_of_bool bits)
        val regs = rand_regs gen
    in subst [{redex = ``reg:RName -> word32``, residue = regs},
              {redex = ``m:word32 -> word8``, residue = mem},
              {redex = ``flagN:bool``, residue = N},
              {redex = ``flagZ:bool``, residue = Z},
              {redex = ``flagC:bool``, residue = C},
              {redex = ``flagV:bool``, residue = V}] s
    end

val basic_state =
    ``<|
          AIRCR := <| ENDIANNESS := F;
                      SYSRESETREQ := F;
                      VECTCLRACTIVE := F;
                      (* Anything else for VECTKEY is "UNPREDICTABLE." *)
                      VECTKEY := 0x05FAw |>;
      CCR := ccr;
      (* Does SPSEL really matter? *)
      CONTROL := <| SPSEL := F; nPRIV := npriv |>;
      CurrentMode := Mode_Thread;
      ExceptionActive := exact;
      MEM := m;
      NVIC_IPR := ipr;
      PRIMASK := pri;
      PSR := ^basic_flags;
      REG := reg;
      SHPR2 := shpr2;
      SHPR3 := shpr3;
      VTOR := vtor;
      count := 0;
      exception := NoException;
      pcinc := pcinc;
      pending := pending
                     |>``;


val smt_rewrites = [m0_stepTheory.Aligned,m0_stepTheory.Align]
val smt_simps =
  (* Need to normalise register names at this stage so that we can check for
     occurances of the stack pointers. *)
  [m0_stepTheory.R_name_def]

(* Partially apply thumb_step_hex because it does some specialization that
   we don't want to repeat. *)
val step = m0_stepLib.thumb_step_hex (false,false);
fun step_code s = step (hd (m0AssemblerLib.m0_code s));

(* The disassembler needs the half words of 32bit instructions to be split up. *)
fun breakup_hex s =
  if String.size s <= 4 then s else
  let val i = String.size s  - 4
  in if String.sub (s, i-1) = #" " then s else
     String.substring (s,0,i) ^ " " ^ String.substring (s,i,4)
  end

fun print_disassembled_hex s = m0AssemblerLib.print_m0_disassemble [QUOTE (breakup_hex s)];

val split_state_thm = prove(``P s ==>
   (aircr = s.AIRCR) ==>
   (ccr = s.CCR) ==>
   (control = s.CONTROL) ==>
   (currentmode = s.CurrentMode) ==>
   (exceptionactive = s.ExceptionActive) ==>
   (smem = s.MEM) ==>
   (nvic_ipr = s.NVIC_IPR) ==>
   (primask = s.PRIMASK) ==>
   (psr = s.PSR) ==>
   (reg = s.REG) ==>
   (shpr2 = s.SHPR2) ==>
   (shpr3 = s.SHPR3) ==>
   (vtor = s.VTOR) ==>
   (scount = s.count) ==>
   (sexception = s.exception) ==>
   (spcinc = s.pcinc) ==>
   (spending = s.pending) ==>
   P (m0_state aircr ccr control currentmode exceptionactive smem nvic_ipr
               primask psr reg shpr2 shpr3 vtor scount sexception spcinc
               spending)``,
  Cases_on `s` THEN (SIMP_TAC (srw_ss()) []));

fun encode code =
    let val hex = m0AssemblerLib.m0_code code
    in map (fn h => (h, if String.size h > 4 then 4 else 2)) hex
    end

fun choose_thms gen thmss =
    map (fn thms => RandGen.genint gen (length thms - 1)) thmss

end

end
