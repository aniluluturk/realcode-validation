structure Logging : Logging =
struct

open Feedback

type outfile = {
  out : TextIO.outstream,
  name : string,
  count : int ref
}

type state = {
  dir : string,
  random_dir : string,
  summaryfile : TextIO.outstream,
  test_info : (string * string * string * string * string * PolyStats.stats * PolyStats.stats list * (Time.time * Time.time) option) option ref,
  stats_info : PolyStats.stats,
  testnum : int ref,
  fail_imposs_comb : outfile,
  fail_smt_unknown : outfile,
  fail_smt_unsat   : outfile,
  fail_mismatch    : outfile,
  fail_misc_exn    : outfile,
  success          : outfile
}

type time = PolyStats.stats
val start_time = PolyStats.get

val separator = "   "

val s : state option ref = ref NONE

fun getstate () =
    case !s of
        NONE => failwith "Tried to use logging outside of test run"
      | SOME s' => s'

fun get_test_info () =
  case !(#test_info (getstate ())) of
      NONE => failwith "Tried to access test information when not executing a test"
    | SOME i => i

fun datetime () =
    Date.fmt "%a, %d %b %Y %H:%M:%S %Z\n" (Date.fromTimeLocal (Time.now()))

fun setup dir info_str =
    let val () =
            case !s of
                SOME _ => failwith "Tried to setup a new test run without finishing the old one"
              | _ => ()
        (* TODO: check for shell metacharacters in dir, because it's hard to
                 make shell commands work properly otherwise *)
        val () = OS.FileSys.mkDir dir
        val rdir = OS.Path.joinDirFile {dir = dir, file="random"}
        val () = OS.FileSys.mkDir rdir
        (* TODO: check this worked *)
        val _  = OS.Process.system ("(git rev-parse HEAD; git status -bs) > " ^ dir ^ "/git-status")
        val summary = TextIO.openOut (OS.Path.joinDirFile {dir = dir, file="summary"})
        val () = TextIO.output (summary, datetime ())
        val () = TextIO.output (summary, info_str)
        val () = TextIO.flushOut summary
        fun newout name = {name = name, count = ref 0,
                           out = TextIO.openOut (OS.Path.joinDirFile {dir = dir, file=name})}
    in s := SOME {dir = dir, random_dir = rdir,
                  summaryfile = summary,
                  testnum = ref 0, test_info = ref NONE,
                  stats_info = PolyStats.get (),
                  fail_imposs_comb = newout "Fail_Imposs_comb",
                  fail_smt_unknown = newout "Fail_SMT_Unknown",
                  fail_smt_unsat = newout "Fail_SMT_Unsat",
                  fail_mismatch = newout "Fail_Mismatch",
                  fail_misc_exn = newout "Fail_MiscExn",
                  success = newout "Success"}
    end

fun filename prefix suffix default =
    case !s of
        NONE => default
      | SOME s =>
        let val n = !(#testnum s)
        in OS.Path.joinDirFile {dir = #dir s, file = prefix ^ Int.toString n ^ suffix}
        end

fun new_test instrs choices harness starttime =
    let fun i_to_s (hex,class,len) = "(\"" ^ hex ^ "\", \"" ^ class ^ "\", " ^ Int.toString len ^ ")"
        val ss = map i_to_s instrs
        val str = "[" ^ String.concatWith ", " ss ^ "]"
        val cs = "[" ^ String.concatWith "," (map Int.toString choices) ^ "]"
        val s = getstate ()
        val n = ! (#testnum s) + 1
        val sn = Int.toString n
        val () = #testnum s := n
        val pretty = map (fn (h,c,_) => h ^ " " ^ c) instrs
        val () = print ("Test " ^ sn ^ ": " ^ String.concatWith " " pretty ^ " branches: " ^ cs ^ "\n")
    in #test_info s := SOME (sn, str, cs, harness, "[]", starttime, [], NONE)
    end

fun split_timing () =
    let val (sn, str, cs, harness, locs, initstats, tailstats, smt) = get_test_info ()
        val new = PolyStats.get ()
    in #test_info (getstate()) := SOME (sn, str, cs, harness, locs, initstats, new::tailstats, smt)
    end

fun smt_times times =
    let val (sn, str, cs, harness, locs, initstats, stats, _) = get_test_info ()
    in #test_info (getstate()) := SOME (sn, str, cs, harness, locs, initstats, stats, SOME times)
    end

fun record_instr_locs locs =
    let val (sn, str, cs, harness, _, initstats, stats,smt) = get_test_info ()
    in #test_info (getstate()) := SOME (sn, str, cs, harness, locs, initstats, stats,smt)
    end

fun report_split_times init l =
  "times:" ^ PolyStats.split_times (init::rev l)

fun report_smt NONE = "nosmt"
  | report_smt (SOME (user,sys)) =
    "smt:" ^ Time.toString user ^ "s+" ^ Time.toString sys ^ "s"

datatype complaint =
  Impossible_combination
| SMT_Unknown
| SMT_Unsat
| Mismatch of string
| Misc_exn of exn

fun c_to_file c =
    let val s = getstate ()
    in case c of
           Impossible_combination => #fail_imposs_comb s
         | SMT_Unknown => #fail_smt_unknown s
         | SMT_Unsat   => #fail_smt_unsat s
         | Mismatch _  => #fail_mismatch s
         | Misc_exn _  => #fail_misc_exn s
    end

fun extra_fail (Mismatch s) = s
  | extra_fail (Misc_exn e) = exn_to_string e
  | extra_fail _ = ""

fun fail complaint =
    let val () = split_timing ()
        val out = c_to_file complaint
        val s = extra_fail complaint
        (* Avoid extra newlines in output *)
        val s = String.toString s
        val () = #count out := !(#count out) + 1
        val (id,ins,cs,harness,locs,stats,splits,smt) = get_test_info ()
        val () = TextIO.output (#out out, (id ^ separator ^ ins ^ separator ^
                                           cs ^ separator ^ harness ^ separator ^
                                           s ^ separator ^ locs ^ separator ^
                                           report_split_times stats splits ^ separator ^
                                           report_smt smt ^ "\n"))
        val () = print ("Failed: " ^ #name out ^ "\n")
        val () = #test_info (getstate()) := NONE
    in TextIO.flushOut (#out out)
    end

fun success () =
    let val () = split_timing ()
        val s = getstate ()
        val out = #success s
        val () = #count out := !(#count out) + 1
        val (id,ins,cs,harness,locs,stats,splits,smt) = get_test_info ()
        val () = TextIO.output (#out out, id ^ separator ^ ins ^ separator ^
                                          cs ^ separator ^ harness ^ separator ^ 
                                          locs ^ separator ^
                                          report_split_times stats splits ^ separator ^
                                          report_smt smt ^ "\n")
        val () = print "OK\n"
        val () = #test_info (getstate()) := NONE
    in TextIO.flushOut (#out out)
    end

fun record_random gen =
    let val s = getstate ()
        val (id,_,_,_,_,_,_,_) = get_test_info ()
        val out = TextIO.openOut (OS.Path.joinDirFile {dir= #random_dir s, file=id})
        val () = TextIO.output (out, RandGen.record gen)
    in TextIO.closeOut out
    end

fun finish () =
    (let val s = getstate()
         val stats = PolyStats.get ()
        fun fin out = 
            let val () = TextIO.output (#summaryfile s,
                                        "Count for " ^ #name out ^ ": " ^
                                        Int.toString (!(#count out)) ^ "\n")
            in TextIO.closeOut (#out out)
            end
        val () = fin (#fail_imposs_comb s)
        val () = fin (#fail_smt_unknown s)
        val () = fin (#fail_smt_unsat s)
        val () = fin (#fail_mismatch s)
        val () = fin (#fail_misc_exn s)
        val () = fin (#success s)
        val () = TextIO.output (#summaryfile s, datetime ())
        val () = TextIO.output (#summaryfile s, PolyStats.format_diff (#stats_info s) stats)
    in TextIO.closeOut (#summaryfile s)
    end; s := NONE)

end
