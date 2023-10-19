structure Desugar : sig

  val desugar : Sugary.term -> Desugared.term
  val int_desugared : int -> Desugared.term

end = struct

  structure S = Sugary
  structure D = Desugared

  fun int_desugared(i) = (case i of
                          0 => D.Zero
                         | _ => D.Succ((int_desugared (i - 1)))
                          )

  fun desugar(st)= (case st of
                      S.Nat(a) => int_desugared(a)
                    | S.True => D.Succ(D.Zero)
                    | S.False => D.Zero
                    | S.Unit => D.Zero
                    | S.Add(a, b) => D.Add((desugar a), (desugar b))
                    | S.Subtract(a, b) => D.Subtract((desugar a), (desugar b))
                    | S.Less(a, b) => D.Less((desugar a),(desugar b))
                    | S.Cond(a, b, c) => D.Cond((desugar a),(desugar b),(desugar c))
                    | S.First(a) => D.First((desugar a))
                    | S.Second(a) => D.Second((desugar a))
                    | S.Pair(a, b) => D.Pair((desugar a), (desugar b))
 (* rewrites VVV *) | S.Eq(a , b) => D.Eq((desugar a),(desugar b))
                    | S.LessEq(a, b) => desugar(S.Or(S.Less(a, b), S.Eq(a, b)))
                    | S.GreaterEq(a, b) => desugar(S.Or(S.Not(S.Less(a, b)), S.Eq(a,b)))
                    | S.Greater(a, b) => desugar(S.Not(S.Less(a, b)))
                    | S.Not(a) => D.Cond((desugar a), D.Zero, D.Succ(D.Zero))
                    | S.And(a, b) => D.Cond((desugar a), (desugar b), (desugar a))
                    | S.Or(a, b) => D.Cond((desugar a), (desugar a), (desugar b))
                    | S.Xor(a, b) => desugar(S.Or(S.And(a, S.Not(b)), S.And(S.Not(a), b))))


                    

      
end
