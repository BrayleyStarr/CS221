structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR


  fun fetch_tuple(r, lbl) = (case r of 
                                       L.Record(nil) => NONE
                                      |L.Record((a, b) :: t) => if a = lbl then SOME((a, b))
                                                               else 
                                                               fetch_tuple(L.Record(t), lbl))


  fun eval(t) = (case t of 
                    L.Int(i) => L.Int(i)
                  | L.True => L.True
                  | L.False => L.False
                  | L.Unit => L.Unit
              (* | Var of string*)
                  | L.Lam(str, typ, trm) => L.Lam(str, typ, trm)
                  | L.App(trm1, trm2) => (case (eval(trm1), eval(trm2)) of 
                                            (L.Lam(str, typ, trm), b) => eval(L.Let(str, b, trm)))
                  
                  | L.Fix(trm) => (case eval(trm) of 
                                    L.Lam(str, typ, trm1) => (case eval(L.Let(str, L.Lam(str, typ, trm1), trm1)) of
                                                                  v => v))
                  | L.Let(str, trm1, trm2) => (case eval(trm1) of 
                                                  v => eval(L.Let(str, v, trm2)))
                  | L.Cond(t1, t2, t3) => (case eval(t1) of 
                                          L.True => eval(t2)
                                         |L.False => eval(t3))
                  | L.Add(trm1, trm2) => (case (eval(trm1), eval(trm2)) of 
                                            (L.Int(a), L.Int(b)) => L.Int(a + b))
                  

                  | L.Sub(trm1, trm2) =>  (case (eval(trm1), eval(trm2)) of 
                                            (L.Int(a), L.Int(b)) => L.Int(a - b))
                  | L.Mul(trm1, trm2) =>  (case (eval(trm1), eval(trm2)) of 
                                            (L.Int(a), L.Int(b)) => L.Int(a * b))
                  | L.Eq(trm1, trm2) =>  (case (eval(trm1), eval(trm2)) of 
                                            (L.Int(a), L.Int(b)) => if a = b then L.True else L.False)
                  | L.LessThan(trm1, trm2) =>  (case (eval(trm1), eval(trm2)) of 
                                                  (L.Int(a), L.Int(b))  => if a < b then L.True
                                                                           else L.False)
                  | L.Not(trm) => (case eval(trm) of 
                                    L.True => L.False
                                    | L.False => L.True)
                  | L.Record((a, b) :: c) => L.Record((a, eval(b)) :: nil @ (case eval(L.Record(c)) of 
                                                                         L.Record(v) => v))
                  | L.Select(str, trm) => (case fetch_tuple(eval(trm), str) of 
                                                              SOME(lbl, v) => v
                                                              | NONE => raise Fail "error, lable not in record"
                                                              )
  )

               
end
