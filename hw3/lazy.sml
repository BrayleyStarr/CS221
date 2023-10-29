structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct

open Subst;	 

  fun step(ulct) = (case ulct of
                    ULC.App(ULC.Lam(str, trm1), trm2) => SOME(Subst.subst(str, trm1, trm2))
                   | ULC.App(trm1, trm2) => (case step(trm1) of
                                               SOME(trm3) => SOME(ULC.App(trm3, trm2))
                                              | NONE => NONE)
                   | _ => NONE)
  

                    
                     
  
                     
end
