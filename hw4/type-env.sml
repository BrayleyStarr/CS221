structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = (string * Type.typ) list
               
  val empty = []

  fun lookup(ev, str) = (case ev of
                          [] => NONE
                        | (a, b) :: t => if (String.compare(a, str) = EQUAL) then SOME(b) else lookup(t, str))

  fun extend(ev, str, tp) = (str, tp) :: ev
					  
end
