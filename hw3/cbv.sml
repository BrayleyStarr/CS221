structure CBV : sig

  val isV : ULC.term -> bool 
  val step : ULC.term -> ULC.term option

end = struct

open Subst;

  fun isV(ulc) = (case ulc of
                  ULC.Lam(str, t) => true
                | _ => false)

  fun step(ulc)= 
                  (case ulc of
                    ULC.App(trm1, trm2) => if isV(trm2) then (case trm1 of
                                                               ULC.Lam(str, t) => SOME(Subst.subst(str, t, trm2))
                                                              | _ => NONE) else
                                            if isV(trm1) then (case step(trm2) of 
                                                                NONE => NONE
                                                              | SOME(trm3) => SOME(ULC.App(trm1, trm3)))
                                                        else (case step(trm1) of 
                                                                NONE => NONE
                                                              | SOME(trm3) => SOME(ULC.App(trm3, trm2))))
                     
end
