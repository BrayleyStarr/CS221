structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

open TypeEnv;

  structure S = Sugary
  structure T = Type

  (* tells if types are the same exluding products
     used as a helper function for typeof        *)
  fun equal_type_wo_prdct(t1, t2) = (case (t1, t2) of 
                                      (T.Nat, T.Nat) => true  
                                    | (T.Bool, T.Bool) => true
                                    | (T.Unit, T.Unit) => true
                                    | _ => false)

  fun typeof(ev, st) = (case st of
                        S.Nat(i) => T.Nat
                      | S.True => T.Bool
                      | S.False => T.Bool
                      | S.Unit => T.Unit
                      | S.Add(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Nat
                                          | _ => raise Fail "type error")
                      | S.Subtract(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Nat
                                          | _ => raise Fail "type error")
                      | S.Mul(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Nat
                                          | _ => raise Fail "type error")
                      | S.Pow(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Nat
                                          | _ => raise Fail "type error")
                      | S.Less(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.Greater(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.LessEq(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.GreaterEq(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.Not(t1) => (case typeof(ev, t1) of
                                        T.Bool => T.Bool
                                      | _ => raise Fail "type error")
                      | S.And(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.Or(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.Xor(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                          | _ => raise Fail "type error")
                      | S.Cond(t1, t2, t3) => (case (typeof(ev, t1), typeof(ev, t2), typeof(ev, t3)) of
                                                (T.Bool, a, b) => (case (a, b) of
                                                                    (T.Nat, T.Nat) => T.Nat
                                                                  | (T.Bool, T.Bool) => T.Bool
                                                                  | (T.Unit, T.Unit) => T.Unit
                                                                  | (T.Product(x, y), T.Product(z, q)) => if equal_type_wo_prdct(x, z) andalso equal_type_wo_prdct(y, q) then T.Product(x,y)
                                                                                                          else raise Fail "type error"
                                                                  | _ => raise Fail "type error")
                                              | _ => raise Fail "type error")
                      | S.Eq(t1, t2) => (case (typeof(ev, t1), typeof(ev, t2)) of
                                          (T.Nat, T.Nat) => T.Bool
                                        | _ => raise Fail "type error")   
                      | S.Pair(t1, t2) => T.Product(typeof(ev, t1), typeof(ev, t2))
                      | S.First(S.Pair(t1, t2)) => typeof(ev, t1)
                      | S.Second(S.Pair(t1, t2)) => typeof(ev, t2)
                      | S.Var(str) => (case lookup(ev, str) of 
                                        NONE => raise Fail "type error"
                                      | SOME(t) => t)
                      | S.Let(str, t1, t2) => (case lookup(ev, str) of
                                                  NONE => raise Fail "type error"
                                                | SOME(t) => (case (typeof(ev, t1), t) of 
                                                                    (T.Nat, T.Nat) => typeof(ev, t2)
                                                                  | (T.Bool, T.Bool) => typeof(ev, t2)
                                                                  | (T.Unit, T.Unit) => T.Unit
                                                                  | (T.Product(x, y), T.Product(z, q)) => if equal_type_wo_prdct(x, z) andalso equal_type_wo_prdct(y, q)
                                                                                                          then typeof(ev, t2)
                                                                                                          else raise Fail "type error"
                                                                  | _ => raise Fail "type error"))
                                                                
                      | _ => raise Fail "type error")
                  

end
