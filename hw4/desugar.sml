structure Desugar : sig

  val desugar : Sugary.term -> ULC.term
  val church_numeral : int -> ULC.term

end = struct


  structure S = Sugary
  structure U = ULC
  val fls = U.Lam("t", U.Lam("f", U.Var("f")))
  val tru = U.Lam("t", U.Lam("f", U.Var("t")))
  val pair = U.Lam("f", U.Lam("s", U.Lam("b", U.Var("bfs"))))
  val fst = U.Lam("p", U.App(U.Var("p"), tru))
  val zz = U.App(pair, U.App(fls, fls))
  val snd = U.Lam("p", U.App(U.Var("p"), fls))


  (*good V*)
  val plus = U.Lam("m", U.Lam("n", U.Lam("s", U.Lam("z", U.App(U.Var("ms"), U.Var("nsz"))))))
  val prd = U.Lam("m", U.App(fst, U.Var("msszz")))
  val fst = U.Lam("p", U.App(U.Var("p"), tru))
  val sub = U.Lam("m", U.Lam("n", U.App(U.Var("n"), U.App(prd, U.Var("m")))))
  val mul = U.Lam("m", U.Lam("n", U.App(U.Var("m"), U.App( U.App(plus, U.Var("n")), fls))))

  val ss = U.Lam("p", U.App(pair, U.App(U.App(snd, U.Var("p")), U.App(U.App(plus, fls), U.App(snd, U.Var("p"))))))
  
 
  val iszro = U.Lam("m", U.App(U.Var("m"), U.App(U.Lam("x", fls), tru)))

 


  val not = U.Lam("b", U.App(U.Var("b"), U.App(fls, tru)))
  val greater = U.App(not, U.App(iszro, U.App(sub, U.Var("xy"))))
  val or = U.Lam("b", U.Lam("c", U.App(U.App(U.Var("b"), tru), U.Var("c") )))
  val less = U.App(not, greater)
  val eq = U.Lam("m", U.Lam("n", U.App(U.App(iszro,U.App(U.Var("m"), U.App(prd, U.Var("n")))),U.App(iszro, U.App(U.Var("n"), U.App(prd, U.Var("m")))))))


  fun church_numeral(i) = (case i of
                             0 => U.Var("z")
                            | _ => U.App(U.Var("s"), church_numeral(i - 1)))

  fun desugar(st) =
                  (case st of
                    S.Nat(int) => U.Lam("s", U.Lam("z", church_numeral(int)))
                  | S.True => U.Lam("t", U.Lam("f", U.Var("t")))
                  | S.False => U.Lam("t", U.Lam("f", U.Var("f")))
                  | S.Unit => U.Lam("t", U.Lam("f", U.Var("f")))
                  | S.Add(t1, t2) => U.App(plus, U.App(desugar(t1), desugar(t2)))
                  | S.Subtract(t1, t2) => U.App(sub, U.App(desugar(t1), desugar(t2)))
                  | S.Mul(t1, t2) => U.App(U.Lam("m", U.Lam("n", U.App(U.Var("m"), U.App( U.App(plus, U.Var("n")), fls)))), U.App(desugar(t1), desugar(t2)))
                  | S.Pow(t1, t2) => U.App(U.Lam("m", U.Lam("n", U.App(U.Var("m"), U.App(U.App(mul, U.Var("n")), tru)))), U.App(desugar(t1), desugar(t2)))

                  (*need to check vvv *)
                  | S.First(t) => U.App(fst, desugar(t))
                  | S.Second(t) => U.App(snd, desugar(t))
                  | S.Pair(t1, t2) => U.App(pair, U.App(desugar(t1), desugar(t2)))
                  | S.And(t1, t2) => U.App(U.Lam("b", U.Lam("c", U.App(U.Var("bc"), fls))), U.App(desugar(t1), desugar(t2)))
                  | S.Or(t1, t2) => U.App(U.Lam("b", U.Lam("c", U.App(U.App(U.Var("b"), tru), U.Var("c") ))), U.App( desugar(t1), desugar(t2)))
                  | S.Not(t) => U.App(U.Lam("b", U.App(U.Var("b"), U.App(fls, tru))), desugar(t))
                  | S.Var(str) => U.Var(str)
                  | S.Eq(t1, t2) =>  U.App(U.Lam("m", U.Lam("n", U.App(U.App(iszro,U.App(U.Var("m"), U.App(prd, U.Var("n")))),U.App(iszro, U.App(U.Var("n"), U.App(prd, U.Var("m"))))))), U.App(desugar(t1), desugar(t2)))
                  | S.Cond(t1, t2, t3) => U.App((U.Lam("l", U.Lam("m", U.Lam("n", U.Var("lmn")))), U.App(desugar(t1), U.App(desugar(t2), desugar(t3)))))
                  | S.Greater(t1, t2) => U.App(U.App(not, U.App(iszro, U.App(sub, U.Var("xy")))), U.App(desugar(t1), desugar(t2)))
                  | S.Less(t1, t2) => U.App(U.App(not, greater), U.App(desugar(t1), desugar(t2)))
                  | S.LessEq(t1, t2) => U.App(or, U.App(U.App(less,U.App(desugar(t1), desugar(t2))) , U.App(eq, U.App(desugar(t1), desugar(t2)))))
                  | S.GreaterEq(t1, t2) => U.App(or, U.App(U.App(greater,U.App(desugar(t1), desugar(t2))) , U.App(eq, U.App(desugar(t1), desugar(t2)))))
                  | S.Xor(t1, t2) => U.App(U.Lam("x", U.Lam("y", U.App(U.App(U.Var("x"), U.App(not, U.Var("y"))) ,U.App(U.Var("y"), U.App(not, U.Var("x")))))), U.App(desugar(t1), desugar(t2)))
                  | S.Let(str, t1, t2) => U.App(U.Lam(str, desugar(t2)), desugar(t2))
) 
  

               


              
                  
                  
                  
                 
                  
                 
               
          

                   (* zz = pair c0 c0;
                    ss = λp. pair (snd p) (plus c1 (snd p));
                    prd = λm. fst (m ss zz))
                    pair = λf.λs.λb. b f s;
                    fst = λp. p tru;
                    snd = λp. p fls;
                   
                       power2 = λm. λn. m n;
                       test = λl. λm. λn. l m n;
                       def greater x y = not (iszero (sub x y))
                       
                       equal = λm. λn.
                        and (iszro (m prd n))
                        (iszro (n prd m));

                        or = λb. λc. b tru c;
                        not = λb. b fls tru;
                        and = λb. λc. b c fls;
                       
                       *)   
                  
             
  

end
