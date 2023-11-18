structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ

  val fetch_tuple_from_label : Type.typ * string -> (string * Type.typ) option
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv
		  
  (* returns NONE if the label lbl doesn't exist in the record r
   returns the tuple associated with the label if it does      *)

  fun fetch_tuple_from_label(r, lbl) = (case r of 
                                       T.Record(nil) => NONE
                                      |T.Record((a, b) :: t) => if a = lbl then SOME((a, b))
                                                               else 
                                                               fetch_tuple_from_label(T.Record(t), lbl))

  fun subty(typ1, typ2) = (case (typ1, typ2) of 
                            (T.Bool, T.Bool) => true 
                          | (T.Int, T.Int) => true
                          | (T.Unit, T.Unit) => true 
                          | (T.Function(a, b), T.Function(c, d))  => if subty(c, a) andalso subty(b, d) 
                                                                     then true 
                                                                     else false
                          | (T.Record(a :: b), T.Record(nil)) => true
                          | (T.Record(a :: b), T.Record((c, d ):: e)) => (case fetch_tuple_from_label(T.Record(a :: b), c ) of                            
                                                                          NONE => false
                                                                        | SOME((f, g)) => subty(g, d) andalso subty(T.Record(a :: b), T.Record(e)))

                          | _ => false
                          )

  fun commonSupertype(t1, t2) = if subty(t1, t2) then SOME(t2)
                                else if subty(t2, t1) then SOME(t1)
                                else NONE

   


  fun typeof_aux(t, env)=

    let 

    fun update_enviro(t, e) = (case t of 
                            L.Lam(str, typ, trm) => E.extend(e, str, typ)
                          | L.Let(str, trm1, trm2) => E.extend(e, str, typeof_aux(trm1, env)) 
                          | _ => e
                          )
    
    val env = update_enviro(t, env)


     
    in

                (case t of
                 L.Int(i) => T.Int
                |L.True => T.Bool
                |L.False => T.Bool
                |L.Unit => T.Unit
                | L.Eq(i1, i2) => (case (typeof_aux(i1, env), typeof_aux(i2, env)) of
                                    (T.Int, T.Int) => T.Bool
                                  | _ => raise Fail "Ill typed eq. Operands are not Ints")
                | L.LessThan(i1, i2) => (case (typeof_aux(i1, env), typeof_aux(i2, env)) of
                                    (T.Int, T.Int) => T.Bool
                                  | _ => raise Fail "Ill typed lessthan. Operands are not Ints")
                | L.Not(trm) => (case typeof_aux(trm, env) of 
                                T.Bool => T.Bool
                               | _ => raise Fail "Ill typed not term")
                | L.Add(i1, i2) => (case (typeof_aux(i1, env), typeof_aux(i2, env)) of
                                         (T.Int, T.Int) => T.Int
                                         | _ => raise Fail "Ill typed plus. Operands are not Ints")
                | L.Sub(i1, i2) => (case (typeof_aux(i1, env), typeof_aux(i2, env)) of
                    (T.Int, T.Int) => T.Int
                    | _ => raise Fail "Ill typed sub. Operands are not Ints")
                | L.Mul(i1, i2) => (case (typeof_aux(i1, env), typeof_aux(i2, env)) of
                                     (T.Int, T.Int) => T.Int
                                    | _ => raise Fail "Ill typed mul. Operands are not Ints")
                | L.Fix(trm) => (case typeof_aux(trm, env) of
                                    T.Function(a, b) => if subty(a, b) andalso subty(b, a) then a else raise Fail "Ill typed fix" 
                                    | _ =>  raise Fail "Ill typed fix")
                | L.App(trm1, trm2) => (case (typeof_aux(trm1, env), typeof_aux(trm2, env)) of
                                            (T.Function(a, b), c) => if subty(c, a) 
                                                                     then b 
                                                                     else raise Fail "Ill typed app"
                                           | _ =>  raise Fail "Ill typed app")
                                            
                | L.Lam(str, typ, trm) => T.Function(typ, typeof_aux(trm, env))
                | L.Cond(trm1, trm2, trm3) => (case (typeof_aux(trm1, env), typeof_aux(trm2, env), typeof_aux(trm3, env)) of 
                                               (T.Bool, b, c) => (case commonSupertype(b, c) of 
                                                                    NONE => raise Fail "Ill typed Cond"
                                                                  | SOME(d) => d)
                                              | _ => raise Fail "Ill typed Cond")
                | L.Record((a, b) :: c) => (case RecordCheck.check(L.Record((a, b) :: c)) of
                                              L.Record((a, b) :: c ) => 
                                           (case c of 
                                             nil => T.Record((a, typeof_aux(b, env)) :: nil)
                                            |_ => T.Record((a, typeof_aux(b, env)) :: nil @ (case typeof_aux(L.Record(c), env) of 
                                                                          T.Record(lst) => lst))))
                | L.Select(str, L.Record(tlst)) => (case fetch_tuple_from_label(typeof_aux(L.Record(tlst), env), str) of 
                                                    SOME(a, b) => b)
                | L.Var(str) => (case TypeEnv.lookup(env, str) of
                                NONE => raise Fail "Variable not found in type-environment"
                              | SOME(d) => d)
                | L.Let(str, trm1 , trm2) => typeof_aux(trm2, env)
                | _ => raise Fail "error")
        end;

                
        fun typeof(t) = 
          let 
            val enviro = E.empty
          in
            typeof_aux(t, enviro )
          end;

  
             

          
end
