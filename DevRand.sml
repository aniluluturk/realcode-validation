structure DevRandBasic : BasicRand =
struct
type gen = BinIO.instream
type init = unit
val rng =  "/dev/urandom"
fun newgen () = BinIO.openIn rng

local open Word8 in
fun byte_bits w =
    [andb (w, 0wx80) <> 0w0,
     andb (w, 0wx40) <> 0w0,
     andb (w, 0wx20) <> 0w0,
     andb (w, 0wx10) <> 0w0,
     andb (w, 0wx08) <> 0w0,
     andb (w, 0wx04) <> 0w0,
     andb (w, 0wx02) <> 0w0,
     andb (w, 0wx01) <> 0w0]
end

fun bits gen bits =
    let val bytes_needed = (bits+7) div 8
        val bytes = BinIO.inputN (gen, bytes_needed)
        val () = if Word8Vector.length bytes < bytes_needed
                 then Feedback.failwith ("End of random number input file: " ^ rng)
                 else ()
        val allbits = Word8Vector.foldl (fn (w,acc) => (byte_bits w)@acc) [] bytes
        val excess_bits = bytes_needed * 8 - bits
    in List.drop (allbits, excess_bits)
    end

fun genint gen max =
    let val bytes_needed = (IntInf.log2 max+8) div 8
        val bytes = BinIO.inputN (gen, bytes_needed)
        val () = if Word8Vector.length bytes < bytes_needed
                 then Feedback.failwith ("End of random number input file: " ^ rng)
                 else ()
        val total = Word8Vector.foldl (fn (w,n) => n*256 + Word8.toInt w) 0 bytes
    (* TODO: is this too biased towards lower values? *)
    in total mod (max+1)
    end

fun select l =
    let val ln = length l
    in
     fn gen => let val i = genint gen (ln-1) in (i,List.nth (l,i)) end
    end

fun dispose gen = BinIO.closeIn gen

end

structure DevRand :> Rand where type init = unit = RandRecord(DevRandBasic)
