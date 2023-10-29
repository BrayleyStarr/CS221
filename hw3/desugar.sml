structure Desugar : sig
 
  val int_to_dst_helper : int -> string
  val int_to_dst : int -> ULC.term
  val desugar : Sweetl.term -> ULC.term

end = struct

  fun int_to_dst_helper(i) = (case i of 
                                0 => "x"
                               | _ => "f" ^ int_to_dst_helper(i - 1))

  (*takes in an int and converts it to ulc desugared term *)
  fun int_to_dst(i) = ULC.Lam("f", ULC.Lam("x", ULC.Var(int_to_dst_helper(i) )))


  fun desugar(st)= (case st of
                    Sweetl.Var(str) => ULC.Var(str)
                  | Sweetl.App(trm1, trm2) => ULC.App(desugar(trm1), desugar(trm2))
                  | Sweetl.Lam(str, trm1) => ULC.Lam(str, desugar(trm1))
                  | Sweetl.Nat(i) => int_to_dst(i)
                  | Sweetl.Abbr(str) => raise Fail "Term should include no abbreviations since it should be unrolled!"
                  | Sweetl.ID(str) => ULC.Lam(str, ULC.Var(str))
                  | Sweetl.Tru => ULC.Lam("t" , ULC.Lam("f", ULC.Var("t")))
                  | Sweetl.Fls => ULC.Lam("t", ULC.Lam("f", ULC.Var("f"))))

  
                  
                         

end
