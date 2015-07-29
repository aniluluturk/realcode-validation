open HolKernel Parse boolLib bossLib

val _ = new_theory "testing";

(* For CombineSteps *)

(* Definition for executing n steps.
   Note that we don't want the definition to be unfolded by computeLib, so use
   zDefine. *)

val NStates_def =
    zDefine `(NStates 0 s = s) /\
             (NStates (SUC n) s = NStates n (THE (^(Target.nextstate) s)))`;

(* For YicesSat *)
val w2n8 = Define `w2n8 (w:word8) = w2n w`

(* For StepSMT *)

(* Avoids a round-trip through num, which I think would be awkward for SMT solving *)
(*val myb2w_def = Define `(myb2w T = 1w) /\ (myb2w F = 0w)`;*)
val myb2w_def = Define `myb2w b = if b then 1w else 0w : 'a word`;

val _ = export_theory ();
