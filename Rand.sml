(* Functions to provide random values.  Mildly abstracted so that it can be
   retrofitted with a better source and/or replay later. *)

signature BasicRand =
sig
    type gen
    type init
    val newgen : init -> gen
    val bits : gen -> int -> bool list
    val select : 'a list -> gen -> (int * 'a)
    val genint : gen -> int -> int
    val dispose : gen -> unit
end

signature Rand =
sig
  include BasicRand

    (* For rerunning particular cases *)
    val record : gen -> string
    val replay : string -> gen
end

functor RandRecord (R:BasicRand) : Rand =
struct
  open Feedback

  datatype record =
           Bits of bool list
         | Select of int * int
         | Int of int * int
  datatype gen =
           Recording of record list ref * R.gen
         | Replaying of record list * record list ref
  type init = R.init

  fun newgen init = Recording (ref [], R.newgen init)
  fun dispose (Recording (_,g)) = R.dispose g
    | dispose (Replaying _) = ()
  fun bits (Recording (r,gen)) i =
      let val bs = R.bits gen i
      in (r := (Bits bs) :: !r; bs)
      end
    | bits (Replaying (_,r)) i =
      case !r of
          (Bits bs :: t) =>
          if length bs = i
          then (r := t; bs)
          else failwith "Requested wrong number of bits in random replay"
        | _ => failwith "Requested wrong kind of randomness in replay"

  fun select l (Recording (r,gen)) =
      let val (j,x) = R.select l gen
      in (r := (Select (length l,j)) :: !r; (j,x))
      end
    | select l (Replaying (_,r)) =
      case !r of
          (Select (i,j):: t) =>
          if length l = i
          then (r := t; (j, List.nth (l,j)))
          else failwith "Requested wrong selection in random replay"
        | _ => failwith "Requested wrong kind of randomness in replay"

  fun genint (Recording (r,gen)) i =
      let val j = R.genint gen i
      in (r := (Int (i,j)) :: !r; j)
      end
    | genint (Replaying (_,r)) i =
      case !r of
          (Int (i',j):: t) =>
          if i = i'
          then (r := t; j)
          else failwith "Requested wrong integer range in random replay"
        | _ => failwith "Requested wrong kind of randomness in replay"

  fun nybble_hex (false,false,false,false) = "0"
    | nybble_hex (false,false,false, true) = "1"
    | nybble_hex (false,false, true,false) = "2"
    | nybble_hex (false,false, true, true) = "3"
    | nybble_hex (false, true,false,false) = "4"
    | nybble_hex (false, true,false,true ) = "5"
    | nybble_hex (false, true, true,false) = "6"
    | nybble_hex (false, true, true, true) = "7"
    | nybble_hex ( true,false,false,false) = "8"
    | nybble_hex ( true,false,false, true) = "9"
    | nybble_hex ( true,false, true,false) = "A"
    | nybble_hex ( true,false, true, true) = "B"
    | nybble_hex ( true, true,false,false) = "C"
    | nybble_hex ( true, true,false, true) = "D"
    | nybble_hex ( true, true, true,false) = "E"
    | nybble_hex ( true, true, true, true) = "F"

  fun hex_nybble #"0" = SOME [false,false,false,false]
    | hex_nybble #"1" = SOME [false,false,false, true]
    | hex_nybble #"2" = SOME [false,false, true,false]
    | hex_nybble #"3" = SOME [false,false, true, true]
    | hex_nybble #"4" = SOME [false, true,false,false]
    | hex_nybble #"5" = SOME [false, true,false,true ]
    | hex_nybble #"6" = SOME [false, true, true,false]
    | hex_nybble #"7" = SOME [false, true, true, true]
    | hex_nybble #"8" = SOME [ true,false,false,false]
    | hex_nybble #"9" = SOME [ true,false,false, true]
    | hex_nybble #"A" = SOME [ true,false, true,false]
    | hex_nybble #"B" = SOME [ true,false, true, true]
    | hex_nybble #"C" = SOME [ true, true,false,false]
    | hex_nybble #"D" = SOME [ true, true,false, true]
    | hex_nybble #"E" = SOME [ true, true, true,false]
    | hex_nybble #"F" = SOME [ true, true, true, true]
    | hex_nybble _ = NONE

  fun aux_bits [] = []
    | aux_bits [a] = [nybble_hex (a,false,false,false)]
    | aux_bits [a,b] = [nybble_hex (a,b,false,false)]
    | aux_bits [a,b,c] = [nybble_hex (a,b,c,false)]
    | aux_bits (a::b::c::d::t) = (nybble_hex (a,b,c,false)::aux_bits t)

  fun toString (Bits bs) =
      "B" ^ Int.toString (length bs) ^ ":" ^ (String.concat o aux_bits) bs
    | toString (Select (i,j)) =
      "S" ^ Int.toString i ^ ":" ^ Int.toString j
    | toString (Int (i,j)) =
      "I" ^ Int.toString i ^ ":" ^ Int.toString j

  fun scan_nybbles f str 0 = SOME ([],str)
    | scan_nybbles f str n =
      case f str of
          SOME (c,str) =>
          (case hex_nybble c of
               SOME ny =>
               if n > 4
               then (case scan_nybbles f str (n-4) of
                         SOME (bs,str) => SOME (ny@bs,str)
                       | NONE => NONE)
               else SOME (List.take (ny,n),str)
             | NONE => NONE)
        | NONE => NONE

  fun scan_bits f str =
      case Int.scan StringCvt.DEC f str of
          SOME (length,str) =>
          (case f str of
              SOME (#":",str) =>
              (case scan_nybbles f str length of
                   SOME (bs,str) => SOME (Bits bs,str)
                 | NONE => NONE)
            | _ => NONE)
        | NONE => NONE

  fun scan_ints con f str =
      case Int.scan StringCvt.DEC f str of
          SOME (i,str) =>
          (case f str of
              SOME (#":",str) =>
              (case Int.scan StringCvt.DEC f str of
                  SOME (j,str) => SOME (con (i,j),str)
                | NONE => NONE)
            | _ => NONE)
        | NONE => NONE

  fun scan f str =
      case f str of
          SOME (#"B",str) => scan_bits f str
        | SOME (#"S",str) => scan_ints Select f str
        | SOME (#"I",str) => scan_ints Int f str
        | _ => NONE

  fun scan_list f str acc =
      let val str = StringCvt.skipWS f str
      in case f str of
             SOME _ =>
             (case scan f str of
                  SOME (r,str) => scan_list f str (r::acc)
                | NONE => failwith "Bad random replay string")
           | NONE => rev acc
      end

  fun record (Recording (r,_)) = String.concatWith " " (rev (map toString (!r)))
    | record (Replaying (r,_)) = String.concatWith " " (map toString r)

  fun replay s = 
      let val r = scan_list Substring.getc (Substring.full s) []
      in Replaying (r, ref r)
      end

end

