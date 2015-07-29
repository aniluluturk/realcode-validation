structure HWTest =
struct

local
open HolKernel boolLib
in

fun connect () = ()
fun close () = ()
fun reset () = ()
fun cpu_id () = ""

fun write_registers out s =
    let val regs = Tools.cbv_eval ``^s.gpr``
        fun write r v =
            let val rhi = r div 8
                val rlo = (r mod 8) * 32
                val (r,rhi,rlo) = (Word8.fromInt r, Word8.fromInt rhi, Word8.fromInt rlo)
                val (b8,v) = (v mod 256, v div 256)
                val (b7,v) = (v mod 256, v div 256)
                val (b6,v) = (v mod 256, v div 256)
                val (b5,v) = (v mod 256, v div 256)
                val (b4,v) = (v mod 256, v div 256)
                val (b3,v) = (v mod 256, v div 256)
                val (b2,v) = (v mod 256, v div 256)
                val (b1,v) = (v mod 256, v div 256)

                val () = BinIO.output1 (out,0wx3c)
                val () = BinIO.output1 (out,r)
                val () = BinIO.output1 (out,Word8.fromInt b1)
                val () = BinIO.output1 (out,Word8.fromInt b2)

                val () = BinIO.output1 (out,0wx34 + rhi)
                val () = BinIO.output1 (out,rlo + r)
                val () = BinIO.output1 (out,Word8.fromInt b3)
                val () = BinIO.output1 (out,Word8.fromInt b4)

                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,r)
                val () = BinIO.output1 (out,0wx04 + r*0w8)
                val () = BinIO.output1 (out,0wx38)

                val () = BinIO.output1 (out,0wx34 + rhi)
                val () = BinIO.output1 (out,rlo + r)
                val () = BinIO.output1 (out,Word8.fromInt b5)
                val () = BinIO.output1 (out,Word8.fromInt b6)

                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,r)
                val () = BinIO.output1 (out,0wx04 + r*0w8)
                val () = BinIO.output1 (out,0wx38)

                val () = BinIO.output1 (out,0wx34 + rhi)
                val () = BinIO.output1 (out,rlo + r)
                val () = BinIO.output1 (out,Word8.fromInt b7)
                val () = BinIO.output1 (out,Word8.fromInt b8)

            in ()
            end
        fun reg_val r =
            let val v = Tools.cbv_eval ``^regs ^(wordsSyntax.mk_wordii (r,5))``
                val v = wordsSyntax.uint_of_word v
            in v
            end
        fun write_reg r =
            let val v = reg_val r
            in write r v
            end
        fun write_lohi () =
            let val lo = wordsSyntax.uint_of_word (rand (Tools.cbv_eval ``^s.lo``))
                val hi = wordsSyntax.uint_of_word (rand (Tools.cbv_eval ``^s.hi``))
                val () = write 1 lo
                (* mtlo $1 *)
                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,0wx20)
                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,0wx13)

                val () = write 1 hi
                (* mthi $1 *)
                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,0wx20)
                val () = BinIO.output1 (out,0wx00)
                val () = BinIO.output1 (out,0wx11)
            in ()
            end
        val () = write_lohi ()
        fun aux 32 = ()
          | aux n = let val () = write_reg n in aux (n+1) end
    in aux 1
    end

fun write_memory out bgmem s =
    let val mem = State.decompose_hol_memory (Tools.cbv_eval ``^s.MEM``) bgmem
        val bootmem = Word8Array.array (Target.sram_size, 0wx0)
        val () = List.app (fn (start, content) => 
                              if start < Target.sram_start orelse
                                 start+Vector.length content > Target.sram_start+Target.sram_size
                              then failwith "Memory out of range"
                              else Vector.appi (fn (i,b) =>
                                                   Word8Array.update (bootmem,
                                                                      i-(start-Target.sram_start),
                                                                      Word8.fromInt b)) content) mem
    in BinIO.output (out, Word8Array.vector bootmem)
    end

(* NB: Assumes initial PC is within target range *)
fun write_jump out s =
    let val pc = Tools.cbv_eval ``^s.PC``
        val pc = wordsSyntax.uint_of_word pc
        (* Calculate instruction bytes *)
        val v = IntInf.orb (IntInf.andb (IntInf.~>> (pc, Word.fromInt 2), 0x03ffffff), 0x08000000)
        val (b4,v) = (v mod 256, v div 256)
        val (b3,v) = (v mod 256, v div 256)
        val (b2,v) = (v mod 256, v div 256)
        val (b1,v) = (v mod 256, v div 256)
        val () = BinIO.output1 (out,Word8.fromInt b1)
        val () = BinIO.output1 (out,Word8.fromInt b2)
        val () = BinIO.output1 (out,Word8.fromInt b3)
        val () = BinIO.output1 (out,Word8.fromInt b4)
        (* Delay slot *)
        val z = Word8.fromInt 0
        val () = BinIO.output1 (out,z)
        val () = BinIO.output1 (out,z)
        val () = BinIO.output1 (out,z)
        val () = BinIO.output1 (out,z)
    in ()
    end

fun report_final_regs out th =
    let fun print s = TextIO.output (out, s)
        val () = print "Post-state registers:\n"
        val post = (rhs o concl) th
        val regs = Tools.cbv_eval ``^post.gpr``
        fun conv v =
            let val s = Int.fmt StringCvt.HEX v
                (*val s = String.map Char.toLower s*)
            in StringCvt.padLeft #"0" 16 s
            end
        fun reg r =
            let val v = Tools.cbv_eval ``^regs ^(wordsSyntax.mk_wordii (r,5))``
                val v = wordsSyntax.uint_of_word v
            in print ("Reg " ^ (if r < 10 then " " else "") ^ Int.toString r ^
                      " " ^ conv v ^ "\n")
            end
        fun aux 32 = ()
          | aux n = let val () = reg n in aux (n+1) end
        val pc = Tools.cbv_eval ``^post.PC``
        val pc = wordsSyntax.uint_of_word pc 
        val () = print ("PC     " ^ conv pc ^ "\n")
    in aux 0
    end

fun check testfile regfile =
  let val () = print "Checking HOL model against simulation\n"
      val cmd = "scripts/l3cmp \"" ^ testfile ^ "\" \"" ^ regfile ^ "\""
      val result = OS.Process.isSuccess (OS.Process.system cmd)
      val () = print (if result then "OK\n" else "Failed\n")
  in result
  end

fun run_and_check con bgmem s full_th harness =
    let val mem_name = Logging.filename "test" ".mem" "/tmp/test.mem"
        val () = print ("Writing test case to " ^ mem_name ^ "\n")
        val out = BinIO.openOut mem_name
        val () = write_registers out s
        val () = write_jump out s
        val () = write_memory out bgmem s
        val () =  BinIO.closeOut out
        val reg_name = Logging.filename "test" ".reg" "/tmp/test.reg"
        val () = print ("Dumping expected register contents to " ^ reg_name ^ "\n")
        val out = TextIO.openOut reg_name
        val () = report_final_regs out full_th
        val () = TextIO.closeOut out
        val cmp = check mem_name reg_name
    in if cmp then NONE else SOME "regs"
    end

end
end
