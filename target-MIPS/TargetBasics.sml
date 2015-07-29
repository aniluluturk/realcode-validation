structure TargetBasics : TargetBasics =
struct

fun compset () = mipsLib.mips_compset [(*utilsLib.mk_reg_thm "m0" "PSR", m0_stepTheory.R_name_def*)]

end
