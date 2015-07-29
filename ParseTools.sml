structure ParseTools =
struct

fun exactly (f:(char,substring) StringCvt.reader -> ('b,substring) StringCvt.reader) s what
    : 'b =
    let val ss = Substring.full s
        fun oops () = Feedback.failwith ("Unable to parse " ^what^ ": " ^ s)
    in case f Substring.getc ss of
         SOME (v,rem) => if Substring.isEmpty (StringCvt.skipWS Substring.getc rem)
                         then v else oops ()
       | NONE => oops ()
    end

end
