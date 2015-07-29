structure testingTheory :> testingTheory =
struct
  val _ = if !Globals.print_thy_loads then print "Loading testingTheory ... " else ()
  open Type Term Thm
  infixr -->

  fun C s t ty = mk_thy_const{Name=s,Thy=t,Ty=ty}
  fun T s t A = mk_thy_type{Tyop=s, Thy=t,Args=A}
  fun V s q = mk_var(s,q)
  val U     = mk_vartype
  (*  Parents *)
  local open m0_decompTheory
  in end;
  val _ = Theory.link_parents
          ("testing",
          Arbnum.fromString "1426678834",
          Arbnum.fromString "835300")
          [("m0_decomp",
           Arbnum.fromString "1426678825",
           Arbnum.fromString "750510")];
  val _ = Theory.incorporate_types "testing" [];

  val idvector = 
    let fun ID(thy,oth) = {Thy = thy, Other = oth}
    in Vector.fromList
  [ID("min", "fun"), ID("num", "num"), ID("fcp", "cart"),
   ID("fcp", "bit0"), ID("one", "one"), ID("min", "bool"),
   ID("m0", "m0_state"), ID("bool", "!"), ID("bool", "/\\"),
   ID("num", "0"), ID("min", "="), ID("arithmetic", "BIT1"),
   ID("bool", "COND"), ID("testing", "NStates"),
   ID("arithmetic", "NUMERAL"), ID("m0_step", "NextStateM0"),
   ID("option", "option"), ID("num", "SUC"), ID("option", "THE"),
   ID("arithmetic", "ZERO"), ID("testing", "myb2w"), ID("words", "n2w"),
   ID("words", "w2n"), ID("testing", "w2n8")]
  end;
  local open SharingTables
  in
  val tyvector = build_type_vector idvector
  [TYOP [1], TYOP [4], TYOP [3, 1], TYOP [3, 2], TYOP [3, 3], TYOP [5],
   TYOP [2, 5, 4], TYOP [0, 6, 0], TYV "'a", TYOP [2, 5, 8],
   TYOP [0, 5, 9], TYOP [6], TYOP [0, 11, 11], TYOP [0, 0, 12],
   TYOP [0, 5, 5], TYOP [0, 14, 5], TYOP [0, 6, 5], TYOP [0, 16, 5],
   TYOP [0, 11, 5], TYOP [0, 18, 5], TYOP [0, 0, 5], TYOP [0, 20, 5],
   TYOP [0, 5, 14], TYOP [0, 9, 5], TYOP [0, 9, 23], TYOP [0, 11, 18],
   TYOP [0, 0, 20], TYOP [0, 0, 0], TYOP [0, 9, 9], TYOP [0, 9, 28],
   TYOP [0, 5, 29], TYOP [16, 11], TYOP [0, 11, 31], TYOP [0, 31, 11],
   TYOP [0, 0, 9]]
  end
  val _ = Theory.incorporate_consts "testing" tyvector
     [("w2n8", 7), ("myb2w", 10), ("NStates", 13)];

  local open SharingTables
  in
  val tmvector = build_term_vector idvector tyvector
  [TMV("b", 5), TMV("n", 0), TMV("s", 11), TMV("w", 6), TMC(7, 15),
   TMC(7, 17), TMC(7, 19), TMC(7, 21), TMC(8, 22), TMC(9, 0), TMC(10, 24),
   TMC(10, 25), TMC(10, 26), TMC(11, 27), TMC(12, 30), TMC(13, 13),
   TMC(14, 27), TMC(15, 32), TMC(17, 27), TMC(18, 33), TMC(19, 0),
   TMC(20, 10), TMC(21, 34), TMC(22, 7), TMC(23, 7)]
  end
  local
  val DT = Thm.disk_thm val read = Term.read_raw tmvector
  in
  val op NStates_def =
    DT(["DISK_THM"],
       [read"((%8 (%6 (|%2. ((%11 ((%15 %9) $0)) $0)))) (%7 (|%1. (%6 (|%2. ((%11 ((%15 (%18 $1)) $0)) ((%15 $1) (%19 (%17 $0)))))))))"])
  val op w2n8_def = DT([], [read"(%5 (|%3. ((%12 (%24 $0)) (%23 $0))))"])
  val op myb2w_def =
    DT([],
       [read"(%4 (|%0. ((%10 (%21 $0)) (((%14 $0) (%22 (%16 (%13 %20)))) (%22 %9)))))"])
  end
  val _ = DB.bindl "testing"
  [("NStates_def",NStates_def,DB.Def), ("w2n8_def",w2n8_def,DB.Def),
   ("myb2w_def",myb2w_def,DB.Def)]

  local open Portable GrammarSpecials Parse
    fun UTOFF f = Feedback.trace("Parse.unicode_trace_off_complaints",0)f
  in
  val _ = mk_local_grms [("m0_decompTheory.m0_decomp_grammars",
                          m0_decompTheory.m0_decomp_grammars)]
  val _ = List.app (update_grms reveal) []
  val _ = update_grms
       (UTOFF temp_overload_on)
       ("NStates", (Term.prim_mk_const { Name = "NStates", Thy = "testing"}))
  val _ = update_grms
       (UTOFF temp_overload_on)
       ("w2n8", (Term.prim_mk_const { Name = "w2n8", Thy = "testing"}))
  val _ = update_grms
       (UTOFF temp_overload_on)
       ("w2n8", (Term.prim_mk_const { Name = "w2n8", Thy = "testing"}))
  val _ = update_grms
       (UTOFF temp_overload_on)
       ("myb2w", (Term.prim_mk_const { Name = "myb2w", Thy = "testing"}))
  val _ = update_grms
       (UTOFF temp_overload_on)
       ("myb2w", (Term.prim_mk_const { Name = "myb2w", Thy = "testing"}))
  val testing_grammars = Parse.current_lgrms()
  end
  val _ = Theory.LoadableThyData.temp_encoded_update {
    thy = "testing",
    thydataty = "compute",
    data = "testing.w2n8_def testing.myb2w_def"
  }

val _ = if !Globals.print_thy_loads then print "done\n" else ()
val _ = Theory.load_complete "testing"
end
