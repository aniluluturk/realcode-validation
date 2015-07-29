signature testingTheory =
sig
  type thm = Thm.thm

  (*  Definitions  *)
    val NStates_def : thm
    val myb2w_def : thm
    val w2n8_def : thm

  val testing_grammars : type_grammar.grammar * term_grammar.grammar
(*
   [m0_decomp] Parent theory of "testing"

   [NStates_def]  Definition

       []
      |- (∀s. NStates 0 s = s) ∧
         ∀n s. NStates (SUC n) s = NStates n (THE (NextStateM0 s))

   [myb2w_def]  Definition

       [] |- ∀b. myb2w b = if b then 1w else 0w

   [w2n8_def]  Definition

       [] |- ∀w. w2n8 w = w2n w


*)
end
