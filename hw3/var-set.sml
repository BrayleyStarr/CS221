structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = [] (* <== Change this to something consistent with the new set type. *)

  fun mem(str, st)= (case st of 
                     [] => false
                    | h :: t => (String.compare(str, h) = EQUAL) orelse mem(str, t))

  fun ins(str, st) = if mem(str, st) then st else (str :: st)

  fun rem(str, st) = (case st of
                      [] => []
                    | s :: t => if (String.compare(s, str) = EQUAL) then t else  s :: rem(str, t))

  fun union(s1 : set, s2 : set) = (case (s1, s2) of
                                    ([] , []) => []
                                  |  ([], b) => b
                                  |  (a, []) => a
                                  | (h :: t, h2 :: t2) => h :: h2 :: union(t, t2))
				      
end
