structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next(l) = (case l
      	        of [] => NONE
		| T.Z :: t => SOME (A.Zero , t)
		| T.F :: t => SOME (A.False , t)
	        | T.T :: t => SOME (A.True, t)
		| T.S :: T.Z :: t => SOME (A.Succ(A.Zero), t)
		| T.S :: T.LBrack :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.RBrack :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.Plus :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.Minus :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.LessThan :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.GreaterThan :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.QuestionMark :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.T :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.F :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.P :: t => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: [] => raise Fail "Invalid argument provided for the Succ constructor!"
		| T.S :: T.S :: t => SOME (A.Succ(A.Succ( (case (next t) of
						         SOME(a, b) => a
							 | _ => raise Fail "Error"))) , (case (next t) of
							     	      	   	        SOME(a, b) => b
											| _ => raise Fail "Error"))
	        | T.P :: T.Z :: t => SOME (A.Pred(A.Zero), t)
		| T.P :: T.LBrack :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.RBrack :: t => raise Fail "Invalid argument provided for the Pred connstructor!"
                | T.P :: T.Plus :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.Minus :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.LessThan :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.GreaterThan :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.QuestionMark :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.T :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: T.F :: t => raise Fail "Invalid argument provided for the Pred constructor!"
                | T.P :: [] => raise Fail "Invalid argument provided for the Pred constructor!"
		| T.P :: T.P :: t => raise Fail "Invalid argument provided for the Pred constructor!"
		| T.P :: T.S :: t => SOME (A.Pred(A.Succ( (case (next t) of
		      	     	     	  		       SOME(a, b) => a
							       | _ => raise Fail "Error"))) , (case (next t) of
							       	      	    	 	       SOME(a, b) => b
											       | _ => raise Fail "Error"))
		| T.LBrack :: toks => (case next toks
		                         of SOME (t1, oper::toks') => (case oper
					                                 of T.Plus => SOME( A.Add( t1, (case (next toks') of
									    	      	    	        SOME (a, b) => a
													| _ => raise Fail "Error")) , (case (next toks') of
													       	     	  	       SOME (x, T.RBrack :: y) => y
																       | _ => raise Fail "Error"))
													       	     	  	        
									  | T.Minus => SOME( A.Subtract( t1, (case (next toks') of
                                                                                                        SOME (a, b) => a
                                                                                                        | _ => raise Fail "Error")) , (case (next toks') of
                                                                                                                                       SOME (x, T.RBrack :: y) => y
                                                                                                                                       | _ => raise Fail "Error"))
									  | T.LessThan => SOME( A.Less( t1, (case (next toks') of
                                                                                                        SOME (a, b) => a
                                                                                                        | _ => raise Fail "Error")) , (case (next toks') of
                                                                                                                                       SOME (x, T.RBrack :: y) => y
                                                                                                                                       | _ => raise Fail "Error"))
									  | T.GreaterThan => SOME( A.Greater( t1, (case (next toks') of
                                                                                                        SOME (a, b) => a
                                                                                                        | _ => raise Fail "Error")) , (case (next toks') of
                                                                                                                                       SOME (x, T.RBrack :: y) => y
                                                                                                                                       | _ => raise Fail "Error"))
									  | T.DoubleAmpersand => SOME( A.And( t1, (case (next toks') of
                                                                                                        SOME (a, b) => a
                                                                                                        | _ => raise Fail "Error")) , (case (next toks') of
                                                                                                                                       SOME (x, T.RBrack :: y) => y
                                                                                                                                       | _ => raise Fail "Error"))
									  | T.DoublePipe => SOME( A.Or( t1, (case (next toks') of
                                                                                                        SOME (a, b) => a
                                                                                                        | _ => raise Fail "Error")) , (case (next toks') of
                                                                                                                                       SOME (x, T.RBrack :: y) => y
                                                                                                                                       | _ => raise Fail "Error"))
									  | T.QuestionMark => SOME( A.Cond( t1, (case (next toks') of
									    		      	    	    	 SOME (a, b) => a
														 | _ => raise Fail "Error") , (case (next toks') of
														     	      	   	       SOME(c, T.Colon :: d) => (case (next d) of
																	       	       	       	     	 SOME (z, y) => z
																					 | _ => raise Fail "Error")
																	       | _ => raise Fail "Error")) , (case (next toks') of
																	       	      	    	 	      SOME(m, T.Colon :: l) => (case (next l) of
																					      	      	      	        SOME(r, T.RBrack :: s) => s
																					                                | _ => raise Fail "Error")
																					      | _ => raise Fail "Error"))
		                                                          | _ => raise Fail "Error")
		                          | _ => raise Fail "Error")
		 | _ => raise Fail "Error")



                fun parse(l) = (case (next l) of
		                      NONE => raise Fail "Error"
		    	       	     | SOME(a , []) => a
				     | _ => raise Fail "Error")
                                     
				     	      	    	  
		     
end
