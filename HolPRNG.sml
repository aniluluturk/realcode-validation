structure HolPRNGBasic : BasicRand =
struct
type gen = Random.generator
type init = unit
val newgen = Random.newgen

fun bits gen bits =
    map (fn x => x = 1) (Random.rangelist (0,2) (bits,gen))

fun select l =
    let val ln = length l
    in
     fn gen => let val i = Random.range (0,ln) gen in (i,List.nth (l,i)) end
    end

fun genint gen max =
    Random.range (0,max+1) gen

fun dispose gen = ()

end

structure HolPRNG :> Rand where type init = unit = RandRecord(HolPRNGBasic)
