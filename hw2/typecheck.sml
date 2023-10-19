structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ
  val same_typ : Type.typ * Type.typ -> bool
  

end = struct

  structure S = Sugary
  structure T = Type

  fun same_typ(a, b) = (case (a, b) of 
                          (T.Nat, T.Nat) => true 
                        | (T.Bool, T.Bool) => true
                        | (T.Unit, T.Unit) => true
                        | (T.Product(a, b), T.Product(c, d) ) => same_typ(a, c) andalso same_typ(b, d)
                        | _ => false)
                        
  
  fun typeof(st)  = (case st of 
                      S.True => T.Bool
                    | S.False => T.Bool
                    | S.Nat(a) => T.Nat
                    | S.Unit => T.Unit
                    | S.Add(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Nat
                                                  |_ => raise Fail "incorrect types inserted into addition operator!")
                                        | _ => raise Fail "incorrect types inserted into addition operator!")
                    | S.Subtract(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Nat
                                                  |_ => raise Fail "incorrect types inserted into subtraction operator!")
                                        | _ => raise Fail "incorrect types inserted into subtraction operator!")
                    | S.Less(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into Less operator!")
                                        | _ => raise Fail "incorrect types inserted into Less operator!")
                    | S.Greater(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into Greater operator!")
                                        | _ => raise Fail "incorrect types inserted into Greater operator!")
                    | S.LessEq(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into LessEq operator!")
                                        | _ => raise Fail "incorrect types inserted into LessEq operator!")
                    | S.GreaterEq(a, b) => (case (typeof a) of
                                        T.Nat => (case (typeof b) of 
                                                  T.Nat => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into Greatereq operator!")
                                        | _ => raise Fail "incorrect types inserted into GreaterEq operator!") 
                    | S.Not(a) =>  (case (typeof a) of 
                                     T.Bool => T.Bool
                                     | _ => raise Fail "Not operator supplied with non Boolean typed term") 
                    | S.And(a, b) => (case (typeof a) of
                                        T.Bool => (case (typeof b) of 
                                                  T.Bool => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into And operator!")
                                        | _ => raise Fail "incorCMrect types inserted into And operator!") 
                    | S.Or(a, b) => (case (typeof a) of
                                        T.Bool => (case (typeof b) of 
                                                  T.Bool => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into And operator!")
                                        | _ => raise Fail "incorrect types inserted into And operator!")
                    | S.Xor(a, b) => (case (typeof a) of
                                        T.Bool => (case (typeof b) of 
                                                  T.Bool => T.Bool
                                                  |_ => raise Fail "incorrect types inserted into And operator!")
                                        | _ => raise Fail "incorrect types inserted into And operator!")
                     | S.Cond(a, b, c) => (case (typeof a) of 
                                        T.Bool => if same_typ((typeof b), (typeof c)) then (typeof b) else raise Fail "terms in conditional don't match types!"
                                        | _ => raise Fail "incorrect type inserted into first term in conditional!")


                    | S.Eq(a, b) => if same_typ(typeof(a), typeof(b)) then T.Bool else raise Fail "terms in EQ don't match types!"
                    | S.Pair(a, b) => T.Product((typeof a), (typeof b))
                    | S.First(a) => (case (typeof a) of
                                      T.Product(f, s) => f
                                      | _ => raise Fail "Error"
                                      )
                    | S.Second(a) => (case (typeof a) of
                                      T.Product(f, s) => s
                                      | _ => raise Fail "Error"))

                      
              
end
