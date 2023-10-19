structure Eval : sig

  val isV  : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list
  val isNv : Desugared.term -> bool

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared
  
  fun isNv(dst)= (case dst of 
                    D.Zero => true
                  | D.Succ(a) => isNv(a)
                  | _ => false)

  fun isV(dst)= if (isNv dst) then true else
                 (case dst of
                    D.Pair(a, b) => (case isV(a) of 
                                      true => (case isV(b) of
                                                true => true 
                                                | _ => false)
                                      | false => false)
                    | _ => false)
		 
  fun step(dst) = if isV(dst) then NONE else
                  (case dst of 
                    D.Succ(a) => (case (step a) of 
                                    NONE => NONE
                                   | SOME(b) => SOME(D.Succ(b)))
                   | D.Add(D.Zero, b) => SOME(b)
                   | D.Add(a, b) => (case step(a) of
                                     SOME(c) => SOME(D.Add(c, b))
                                    | NONE => (case a of 
                                               D.Succ(n) =>  if isV(n) then SOME(D.Add(n, D.Succ(b))) else NONE
                                               | _ => NONE))
                   | D.Subtract(a, b) => (case step(a) of
                                           SOME(c) => SOME(D.Subtract(c, b))
                                           | _ => if isV(a) then (case step(b) of   
                                                                      SOME(d) => SOME(D.Subtract(a, d))
                                                                      | _ => (case (a, b) of
                                                                              (D.Zero, x) => if isV(x) then SOME(D.Zero) else NONE
                                                                             |(x, D.Zero) => if isV(x) then SOME(x) else NONE
                                                                             |(D.Succ(x), D.Succ(y)) => if isV(x) andalso isV(y) then SOME(D.Subtract(x, y)) else NONE
                                                                             | _ => NONE)) else NONE)
                    | D.Less(D.Zero, D.Zero) => SOME(D.Zero)
                    | D.Less(a, b) => (case step(a) of    
                                        SOME(c) => SOME(D.Less(c, b))
                                       | NONE => if isV(a) then 
                                                                (case step(b) of 
                                                                   SOME(d) => SOME(D.Less(a, d))
                                                                 | NONE => if isV(b) then (case (a, b) of
                                                                                           (D.Zero, D.Zero) => SOME(D.Zero)
                                                                                         | (D.Zero, D.Succ(v)) => if isV(v) then SOME(D.Succ(D.Zero)) else NONE
                                                                                         | (x, D.Zero) => if isV(x) then SOME(D.Zero) else NONE
                                                                                         | (D.Succ(x), D.Succ(y)) => if isV(x) andalso isV(y) then SOME(D.Less(x, y)) else NONE
                                                                                         | (_ , _) => NONE) else NONE)
                                                                                         else NONE)
                    |D.Eq(a, b) => (case step(a) of
                                    SOME(c) => SOME(D.Eq(c, b))
                                    | NONE => if isV(a) then (case step(b) of
                                                               SOME(d) => SOME(D.Eq(a, d))
                                                              | NONE => if isV(b) then (case (a, b) of  
                                                                                            (D.Zero, D.Zero) => SOME(D.Succ(D.Zero))
                                                                                          | (D.Zero, D.Succ(v)) => if isV(v) then SOME(D.Zero) else NONE
                                                                                          | (D.Succ(v), D.Zero) => if isV(v) then SOME(D.Zero) else NONE
                                                                                          | (D.Succ(x), D.Succ(y)) => if isV(x) andalso isV(y) then SOME(D.Eq(x, y)) else NONE
                                                                                          | (D.Pair(x, y) , D.Pair(z, p)) => if (isV(x) andalso isV(y) andalso isV(z) andalso isV(p)) then SOME(D.Cond(D.Eq(x, z), D.Eq(y, p), D.Zero)) else NONE
                                                                                          |(_, _) => NONE)

                                                                                          else NONE) else NONE)
                    |D.Cond(D.Zero, a, b) => SOME(b)
                    |D.Cond(D.Succ(D.Zero), a, b) => SOME(a)
                    |D.Cond(a, b, c) => (case step(a) of    
                                          SOME(d) => SOME(D.Cond(d, b, c))
                                          | NONE => NONE)
                    |D.Pair(a, b) => (case step(a) of 
                                      SOME(c) => SOME(D.Pair(c, b))
                                      | NONE => if isV(a) then (case step(b) of
                                                  SOME(d) => if isV(a) then SOME(D.Pair(a, d)) else NONE
                                                  | NONE => NONE) else NONE)
                    |D.First(t) => (case step(t) of
                                     SOME(tt) => SOME(D.First(tt))
                                     | NONE => (case t of
                                                 D.Pair(a, b) => if (isV(a) andalso isV(b)) then SOME(a) else NONE
                                                | _ => NONE))
                    |D.Second(t) => (case step(t) of
                                     SOME(tt) => SOME(D.Second(tt))
                                     | NONE => (case t of
                                                 D.Pair(a, b) => if (isV(a) andalso isV(b)) then SOME(b) else NONE
                                                | _ => NONE)) 
                   |_ => NONE)
                     
				    
  fun eval t =
    let
      fun lp t =
	(case step t
	   of SOME t' => t :: lp t'
	    | NONE => [t])
    in
      lp t
    end		    


  fun result(dst) = (case eval(dst) of
                       h :: t :: [] => if isV(t) then Value(t) else Stuck(t)
                     | h :: [] => if isV(h) then Value(h) else Stuck(h)
                     | _ => raise Fail "Error")

  
  

end
