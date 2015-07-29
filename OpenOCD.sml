structure OpenOCD : OpenOCD =
struct

open HolKernel

fun raw_send_command (sk,st) =
  let val vs = Word8VectorSlice.full (Byte.stringToBytes (st ^ str (chr 0x1a)))
  in Socket.sendVec (sk, vs)
  end

val timeout = Time.fromSeconds 120;
exception TimeOut;

type connection = Socket.active INetSock.stream_sock

(* Assume that we get everything in a packet... *)
fun waitForResponse sk =
  let fun get () =
    let val r = Socket.select
        { rds = [Socket.sockDesc sk], wrs = [], exs = [], timeOut = SOME timeout }
    in case #rds r of
         [] => raise TimeOut (* timeout *)
       | _  => Socket.recvVec (sk,65536)
    end
      fun getlots () =
          let val sl = get ()
              val len = Word8Vector.length sl
              val () = if len = 0 then failwith "Connection closed" else ()
          in case Word8Vector.findi (fn (_,x) => x = Word8.fromInt 0x1a) sl of
             SOME (i,_) =>
             if i < len - 1 then failwith "Junk after response"
             else Byte.unpackStringVec (Word8VectorSlice.slice (sl,0,SOME (len-1)))
           | NONE => Byte.bytesToString sl ^ getlots ()
          end
  in  getlots ()
  end

fun connect () =
    let val s : connection = INetSock.TCP.socket()
        val ent = Option.valOf (NetHostDB.getByName "localhost")
        val addr = INetSock.toAddr(NetHostDB.addr ent, 6666)
        val _ = Socket.connect (s,addr)
    in s
    end

fun close con =
    Socket.close con

(* The usual case is to send a command and receive a single response, which
   we log. *)
fun send_command con command =
  let val _ = print (command ^ "\n")
      val _ = raw_send_command (con,command)
      val r = waitForResponse con
  in print (r ^ "\n"); r
  end

fun send_unreported_command con command =
  let val _ = raw_send_command (con,command)
  in waitForResponse con
  end

fun send_quiet_command con command =
  let val _ = print (command ^ "\n")
      val _ = raw_send_command (con,command)
      val r = waitForResponse con
  in if r = "" then ()
     else failwith ("Unexpected response to command " ^ command ^ ": " ^ r)
  end

fun send_unreported_quiet_command con command =
  let val _ = raw_send_command (con,command)
      val r = waitForResponse con
  in if r = "" then ()
     else failwith ("Unexpected response to command " ^ command ^ ": " ^ r)
  end

local
  fun hex x =  Int.fmt StringCvt.HEX x
in
fun mww s addr d = send_quiet_command s ("mww 0x" ^ hex addr ^ " 0x" ^ hex d)
fun mwh s addr d = send_quiet_command s ("mwh 0x" ^ hex addr ^ " 0x" ^ hex d)
fun mwb s addr d = send_quiet_command s ("mwb 0x" ^ hex addr ^ " 0x" ^ hex d)
fun mwb_internal s addr d = send_unreported_quiet_command s ("mwb 0x" ^ hex addr ^ " 0x" ^ hex d)
fun set_reg s n v = send_quiet_command s ("reg " ^ n ^ " 0x" ^ hex v)
end


local
  fun scan_non_WS f str = StringCvt.splitl (not o Char.isSpace) f str
      
  fun scan_reg (f:(char, 'a) StringCvt.reader) : ((string * int), 'a) StringCvt.reader =
   fn str =>
      case scan_non_WS f str of (id, str) =>
      case scan_non_WS f (StringCvt.skipWS f str) of (_, str) =>
      (case f (StringCvt.skipWS f str) of SOME (#"0", str) =>
      (case f (StringCvt.skipWS f str) of SOME (#"x", str) =>
      (case Int.scan StringCvt.HEX f str of SOME (vl, str) =>
        SOME ((id,vl), str)
      | NONE => NONE)
      | _ => NONE)
      | _ => NONE)

fun parse_register s = ParseTools.exactly scan_reg s "register contents"
in
fun get_register con reg =
    let val s = send_command con ("ocd_reg " ^ reg)
    in snd (parse_register s)
    end
end

fun set_memory con start values =
    let val starts = Int.fmt StringCvt.HEX start
        val length = Vector.length values
        val lengths = Int.fmt StringCvt.DEC length
        val _ = 
            print ("Writing memory at 0x" ^ starts ^ " for " ^ lengths ^ " bytes\n")
        fun send off l =
            let val bytes = List.tabulate (l, fn i =>
                                Int.fmt StringCvt.DEC i ^ " 0x" ^
                                Int.fmt StringCvt.HEX (Vector.sub (values ,off+i)))
                val _ = send_unreported_command con
                                       ("set m {" ^ String.concatWith " " bytes ^ "}")
                val start = Int.fmt StringCvt.HEX (start+off)
                val len = Int.fmt StringCvt.DEC l
            in send_unreported_quiet_command con ("array2mem m 8 0x" ^ start ^ " " ^ len)
            end
        val batch = 256
        fun aux off =
            if off + batch >= length then send off (length-off)
            else (send off batch; aux (off+batch))
    in aux 0
    end

fun get_byte_internal con addr =
    let val _ = send_unreported_quiet_command con ("mem2array m 8 " ^ int_to_string addr ^ " 1")
        val s = send_unreported_command con "set m(0)"
    in ParseTools.exactly (Int.scan StringCvt.DEC) s "memory read"
    end

fun get_byte con addr =
    let val () = print ("Reading byte at 0x" ^ Int.fmt StringCvt.HEX addr)
        val r = get_byte_internal con addr
        val () = print (" = " ^ Int.toString r ^ "\n")
    in r
    end

fun get_word con addr =
    let val () = print ("Reading word at 0x" ^ Int.fmt StringCvt.HEX addr)
        val () = send_unreported_quiet_command con 
                                               ("mem2array m 32 " ^ int_to_string addr ^ " 1")
        val s = send_unreported_command con "set m(0)"
        val r = ParseTools.exactly (Int.scan StringCvt.DEC) s "memory read"
        val () = print (" = 0x" ^ Int.fmt StringCvt.HEX r ^ "\n")
    in r
    end

fun get_memory con start length =
    let val starts = Int.fmt StringCvt.HEX start
        val lengths = Int.fmt StringCvt.DEC length
        val _ = 
            print ("Reading memory at 0x" ^ starts ^ " for " ^ lengths ^ " bytes\n")
        val r = Array.array (length, 0)
        fun recv off l =
            let val start = Int.fmt StringCvt.HEX (start+off)
                val len = Int.fmt StringCvt.DEC l
                val _ = send_unreported_command con "set m { }"
                val _ = send_unreported_quiet_command con
                          ("mem2array m 8 0x" ^ start ^ " " ^ len)
                val bytes = send_unreported_command con "set m"
                fun parse_list f str =
                    case f str of
                        NONE => SOME ((),str)
                      | _ => 
                        (case Int.scan StringCvt.DEC f str of
                             (SOME (i, str)) =>
                             (case Int.scan StringCvt.DEC f (StringCvt.skipWS f str) of
                                  (SOME (v, str)) => (Array.update (r,off+i,v);
                                                      parse_list f str)
                                | NONE => NONE)
                           | NONE => NONE)
            in ParseTools.exactly parse_list bytes "bulk memory read"
            end
        val batch = 256
        fun aux off =
            if off + batch >= length then recv off (length-off)
            else (recv off batch; aux (off+batch))
    in (aux 0; r)
    end

end

(*
cmd "reset halt";
*)

