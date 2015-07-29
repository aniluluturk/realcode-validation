(* Elapsed time *)

fun time' f x =
    let val z = Time.now ()
	val r = f x
	val n = Time.now ()
	val _ = print (Time.toString (n-z) ^ "\n")
    in r
    end
