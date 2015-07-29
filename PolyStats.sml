structure PolyStats =
struct

type stats =
     {gcFullGCs: int,
     gcPartialGCs: int,
     sizeAllocation: int,
     sizeAllocationFree: int,
     sizeHeap: int,
     sizeHeapFreeLastFullGC: int,
     sizeHeapFreeLastGC: int,
     threadsInML: int,
     threadsTotal: int,
     threadsWaitCondVar: int,
     threadsWaitIO: int,
     threadsWaitMutex: int,
     threadsWaitSignal: int,
     timeGCSystem: Time.time,
     timeGCUser: Time.time,
     timeNonGCSystem: Time.time,
     timeNonGCUser: Time.time, userCounters: int vector}

val get : unit -> stats = PolyML.Statistics.getLocalStats

local
  fun mem x =
      let val r = Real.fromInt x
          val m = r / 1024.0 / 1024.0
      in Real.toString m ^ "MB"
      end

  fun time x y = Time.toString (Time.- (y,x)) ^ "s"

in

fun format_diff (x:stats) (y:stats) =
    "Full GCs: " ^ Int.toString (#gcFullGCs y - #gcFullGCs x) ^
    "\nPartial GCs: "  ^ Int.toString (#gcPartialGCs y - #gcPartialGCs x) ^
    "\nHeap pre: " ^ mem (#sizeHeap x) ^ " post: " ^ mem (#sizeHeap y) ^
    "\nGC system time: " ^ time (#timeGCSystem x) (#timeGCSystem y) ^
    "\nGC user time: " ^ time (#timeGCUser x) (#timeGCUser y) ^
    "\nNon GC system time: " ^ time (#timeNonGCSystem x) (#timeNonGCSystem y) ^
    "\nNon GC user time: " ^ time (#timeNonGCUser x) (#timeNonGCUser y) ^
    "\n"

fun short_diff (x:stats) (y:stats) =
    "" ^ time (#timeGCSystem x + #timeGCUser x) (#timeGCSystem y + #timeGCUser y) ^
    "+" ^ time (#timeNonGCSystem x + #timeNonGCUser x) (#timeNonGCSystem y + #timeNonGCUser y)

fun split_times [] = ""
  | split_times [_] = ""
  | split_times [s1,s2] = short_diff s1 s2
  | split_times (s1::s2::t) = short_diff s1 s2 ^ "," ^ split_times (s2::t)

end

end
