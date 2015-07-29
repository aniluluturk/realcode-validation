structure HWTest =
struct

local
open HolKernel boolLib
in

fun connect () = OpenOCD.connect()
fun close debug = OpenOCD.close debug

fun reset debug =
    if Target.xmc_reset then
       (* The code executed at start-up breaks the debugger *)
       (OpenOCD.send_quiet_command debug "halt";
        OpenOCD.send_quiet_command debug "bp 0x10001018 2 hw";
        OpenOCD.send_quiet_command debug "reset";
        (* Hopefully long enough for openocd to reconnect *)
        OS.Process.sleep (Time.fromReal 0.5);
        OpenOCD.send_quiet_command debug "rbp 0x10001018")
    else
       OpenOCD.send_quiet_command debug "reset halt"
                                  
fun init debug =
  let val () = reset debug
      fun set (addr, value, mask) =
          let val orig = OpenOCD.get_word debug addr
              val base = IntInf.andb (orig, IntInf.notb mask)
              val new = IntInf.orb (base, value)
          in
              OpenOCD.send_quiet_command debug
                                     ("mww 0x" ^ Int.fmt StringCvt.HEX addr ^
                                      " 0x" ^ Int.fmt StringCvt.HEX new)
          end
  in List.app set Target.init
  end

fun extract high low v =
    IntInf.~>> (IntInf.andb (v, (IntInf.pow (2,high+1)) - 1), Word.fromInt low)

fun cpu_id debug =
    let val id = OpenOCD.get_word debug 0xE000ED00
        val implementor = extract 31 24 id
        val variant = extract 23 20 id
        val architecture = extract 19 16 id
        val partno = extract 15 4 id
        val revision = extract 3 0 id
    in "Implementor: 0x" ^ Int.fmt StringCvt.HEX implementor ^
       (if implementor = 0x41 then " (ARM)\n" else " (*not* ARM)\n") ^
       "Variant: " ^ Int.fmt StringCvt.DEC variant ^ "\n" ^
       "Architecture: 0x" ^ Int.fmt StringCvt.HEX architecture ^
       (if architecture = 0xC then " (ARMv6-M)\n" else " (*not* ARMv6-M)\n") ^
       "Partno: 0x" ^ Int.fmt StringCvt.HEX partno ^
       (if partno = 0xC20 then " (Cortex-M0)\n" else " (*not* Cortex-M0)\n") ^
       "Revision: " ^ Int.fmt StringCvt.DEC revision ^ "\n"
    end

val registers = [
  (``RName_0``, "r0"),
  (``RName_1``, "r1"),
  (``RName_2``, "r2"),
  (``RName_3``, "r3"),
  (``RName_4``, "r4"),
  (``RName_5``, "r5"),
  (``RName_6``, "r6"),
  (``RName_7``, "r7"),
  (``RName_8``, "r8"),
  (``RName_9``, "r9"),
  (``RName_10``, "r10"),
  (``RName_11``, "r11"),
  (``RName_12``, "r12"),
  (``RName_SP_main``, "msp"),
  (``RName_SP_process``, "psp"),
  (``RName_LR``, "lr"),
  (``RName_PC``, "pc")
];

fun set_registers debug s =
    let val regs = Tools.cbv_eval ``^s.REG``
    in List.app (fn (rname,ocd) =>
                    let val v = Tools.cbv_eval ``^regs ^rname``
                        val v = wordsSyntax.uint_of_word v
                    in OpenOCD.set_reg debug ocd v; ()
                    end) registers
    end

fun get_registers debug offset =
    List.map (fn (rname,ocd) =>
                 let val v = OpenOCD.get_register debug ocd
                     val v = if ocd = "pc" then v - offset else v
                 in (rname,v)
                 end) registers

fun set_psr debug s =
    let val psr = Tools.cbv_eval ``reg'PSR (^s.PSR with psr'rst := 0w)``
        val psr = wordsSyntax.uint_of_word psr
    in OpenOCD.set_reg debug "xPSR" psr; ()
    end

fun get_raw_psr debug =
    let val psr = OpenOCD.get_register debug "xPSR"
    in wordsSyntax.mk_wordii (psr,32)
    end

fun get_psr debug =
    let val psr = OpenOCD.get_register debug "xPSR"
        val psr = wordsSyntax.mk_wordii (psr,32)
    in Tools.cbv_eval ``rec'PSR ^psr``
    end
    handle e => raise wrap_exn "HWTest" "get_psr" e

fun write_memory debug bgmem s =
    let val mem = State.decompose_hol_memory (Tools.cbv_eval ``^s.MEM``) bgmem
    in List.app (fn (start, content) => OpenOCD.set_memory debug start content) mem
    end

fun setup_systick debug =
    (OpenOCD.send_quiet_command debug "mww 0xE000E014 0x00FFFFFF";
    OpenOCD.send_quiet_command debug "mww 0xE000E018 0";
    OpenOCD.send_quiet_command debug "mww 0xE000E010 5")

fun read_systick debug =
    0x00FFFFFF - (OpenOCD.get_word debug 0xE000E018)

fun compare_reg th reg =
    let val model_reg = Tools.cbv_eval ``^((snd o dest_eq o concl) th).REG``
        val reg = map (fn (r,v) => (r,wordsSyntax.mk_wordii (v,32))) reg
    in foldl (fn ((r,v),fails) =>
                    let val x = Tools.cbv_eval ``^model_reg ^r = ^v``
                    in if x = ``T`` then fails
                       else let val r = term_to_string r
                            in print ("Failed register comparison: " ^
                                      r ^ "\n");
                               ("reg:" ^ r)::fails
                            end
                    end) [] reg
    end
    handle e => raise wrap_exn "HWTest" "compare_reg" e


fun compare_mem bgmem th mem =
    let fun cmp addr vl x =
            if Array.sub (mem, addr-Target.sram_start) = vl
            then x (* print ("OK memory comparison at " ^ Int.fmt StringCvt.HEX addr ^ "\n")*)
            else (print ("Failed memory comparison at " ^ Int.fmt StringCvt.HEX addr ^ "\n");
                  false)
        val model_mem = Tools.cbv_eval ``^((snd o dest_eq o concl) th).MEM``
    in if State.fold_memory model_mem bgmem cmp true then [] else ["mem"]
    end
    handle e => raise wrap_exn "HWTest" "compare_mem" e

fun compare_psr th psr =
    let val model_psr = Tools.cbv_eval ``reg'PSR (^((snd o dest_eq o concl) th).PSR with psr'rst := 0w)``
    in if model_psr = psr then []
       else
           let val mod_psr = term_to_string model_psr
               val mea_psr = term_to_string psr
           in print ("Model PSR " ^ mod_psr ^
                   " does not match measured PSR " ^ mea_psr ^ "\n");
              ["psr:mod=" ^ mod_psr ^ ",mea=" ^ mea_psr]
           end
    end
    handle e => raise wrap_exn "HWTest" "compare_psr" e

fun compare_count th count =
    let val model_count = Tools.cbv_eval ``^((snd o dest_eq o concl) th).count``
        val model_count = numSyntax.int_of_term model_count
        val model_string = Int.toString model_count
        val count_string = Int.toString count
    in if count = model_count then []
       else (print ("Failed cycle count comparison, model=" ^ model_string ^
                   ", measured=" ^ count_string ^ "\n");
             ["cycles:mod=" ^ model_string ^ ",mea=" ^ count_string])
    end
    handle e => raise wrap_exn "HWTest" "compare_count" e

fun run_test debug bgmem s harness =
    let val _ = init debug
        val _ = write_memory debug bgmem s
        val _ = set_registers debug s
        val _ = set_psr debug s
        val _ = setup_systick debug
        val _ = OpenOCD.send_command debug "resume"
        val mem = OpenOCD.get_memory debug Target.sram_start Target.sram_size
        val reg = get_registers debug (Harness.pc_offset harness)
        val psr = get_raw_psr debug
        (* Correct for harness timing *)
        val count = (read_systick debug) - (Harness.harness_cost harness)
    in (mem,reg,psr,count)
    end

fun run_and_check con bgmem s full_th harness =
    let val (mem,reg,psr,count) = run_test con bgmem s harness
        val rfail = compare_reg full_th reg
        val memfail = compare_mem bgmem full_th mem
        val psrfail = compare_psr full_th psr
        val countfail = compare_count full_th count
    in if rfail = nil andalso memfail = nil andalso psrfail = nil andalso countfail = nil
       then (print "All comparisons OK.\n"; NONE)
       else SOME (String.concatWith ", " (rfail @ memfail @ psrfail @ countfail))
    end

end
end
