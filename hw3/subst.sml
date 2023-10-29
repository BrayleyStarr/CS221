structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct

open VarSet;

  fun fv(ulct)= (case ulct of
                  ULC.Var(str) => VarSet.ins(str, VarSet.empty)
                | ULC.App(t1, t2) => VarSet.union(fv(t1), fv(t2))
                | ULC.Lam(str, t1) => VarSet.rem(str, fv(t1)))

	(* str is the variable to be replaced
     trm1 is the term to Change
     trm2 is the term to substitute in *)
  fun subst(str, trm1, trm2) = 
        let 
           val fresh = Fresh.var();
        in 
          (case trm1 of
              ULC.Var(s) => if (String.compare(str, s) = EQUAL) then trm2 else trm1
              | ULC.App(t1, t2) => ULC.App(subst(str, t1, trm2), subst(str, t2, trm2))
              | ULC.Lam(s, t1) => if (String.compare(s, str) = EQUAL) then trm1 
                                  else 
                                  if mem(s, fv(trm2)) then subst(str, ULC.Lam(fresh, subst(s, t1, ULC.Var(fresh)))     , trm2)
                                  else ULC.Lam(s, subst(str, t1, trm2)))
        end;

end
