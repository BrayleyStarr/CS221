structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV(t) = (case t of
      	      	   A.Zero => true
		 | A.Succ(t) => (case t of
		   	     	   A.Zero => true
				 | A.Succ(n) => isNV(n)
				 | _ => false)
		 | A.Pred(t) => (case t of
		   	     	   A.Zero => true
		   	     	 | A.Succ(n) => isNV(n)
				 | _ => false)
		 | _ => false)

  fun isV(t) = if (isNV t) then true else (case t of
      	       	  	   	     	   A.True => true
					  |A.False => true
					  | _ => false)
      	       	
		 
  fun step(t) = (case t of
      	      	 A.Pred(s) => (case s of
		  	       	 A.Zero => SOME(A.Zero)
				 | A.Succ(n) => if (isNV n) then SOME(n) else raise Fail "Error" (* Done *)
				 |_ => (case (step s) of
				         SOME(x) => SOME(A.Pred(x))
				       | NONE => NONE))		
				  
                | A.Succ(s) =>  (case (step s) of                                       (* Done *)
		  	         SOME(a) => SOME(A.Succ(a))
				 | NONE => NONE) 
				 
		| A.Add(a, b) => (case a of
		  	      	  A.Zero => SOME(b)
				  | A.Succ(s) => if (isNV s) then SOME(A.Add(s, A.Succ(b))) else raise Fail "Error" (* not sure if right *)
				  | _ => (case (step a) of
				      	 SOME(z) => SOME(A.Add(z, b))
					 | NONE => NONE))    
					 
		| A.Subtract(a, b) => (case a of
		  		      	    A.Zero => if (isNV b) then SOME(A.Zero) else raise Fail "Error"
					    |_ => (case b of
					      	  A.Zero => if (isNV a) then SOME(a) else raise Fail "Error"
						  | _ => (case a of
						      	  A.Succ(s) => (case b of
							  	        A.Succ(n)  => SOME(A.Subtract(s, n))
									| _ => (case (step a) of
									        SOME(w) => SOME(A.Subtract(w, b))
										| _ => if (isV a) then (case (step b) of
										       	       	       	 SOME(z) => SOME(A.Subtract(a, z))
													 | _ => NONE) else NONE))))) 

		| A.Less(a, b) => (case a of
		  	       	   A.Zero => (case b of
				   	      A.Zero => SOME(A.False)
					      |A.Succ(s) => SOME(A.True)
					      | _ => NONE)
			          | A.Succ(z) => (case b of
				    	      	  A.Zero => SOME(A.False)
						  | A.Succ(x) => SOME(A.Less(z, x))
						  | _ => NONE)
			          | _ => (case (step a) of
				      	  SOME(i) => SOME(A.Less(i, b))
					  | _ => (case (step b) of
					      	       SOME(q) => if (isV a) then SOME(A.Less(a, q)) else raise Fail "Error"
						       | NONE => NONE)))
					      	   
				    	      	 
		  	       	   
		| A.Greater(a, b) =>  
		                      (case a of
                                       A.Zero => if (isNV b) then SOME(A.False) else raise Fail "Error"
                                       | A.Succ(s) => (case b of
                                                           A.Zero => if (isNV s) then SOME(A.True) else raise Fail "Error"
                                                          | _ => (case (step a) of
                                                                   SOME(x) => SOME(A.Greater(x, b))
                                                                   | _ => (case (step b) of
                                                                                SOME(z) => if (isV a) then SOME(A.Greater(a, z)) else raise Fail "Error"
                                                                                | NONE => NONE))))
		| A.And(a, b) => (case a of
		  	      	   A.True => SOME(b)
				   | A.False => SOME(A.False)
				   | _ => (case (step a) of
				       	  SOME(c) => SOME(A.And(c, b)) (* done *)
					  | _ => NONE)) 
					  
		| A.Or(a, b) => (case a of
		  	     	 A.True => SOME(A.True)
				 | A.False => SOME(b)        (* done *)
				 | _ => (case (step a) of
				     	 SOME(z) => SOME(A.Or(z, b))
					 | _ => NONE)) 
		
		| A.Cond(a, b, c) => (case a of
		  	       	      A.True => SOME(b)
				     |A.False => SOME(c)   (* done *)
				     |_ => (case (step a) of
				      	    SOME(z) => SOME(A.Cond(z, b, c))
					    | _ => NONE)) 
		| A.Zero => NONE
		| A.True => NONE
		| A.False => NONE)
		

  fun eval _ = (case (step t) of
                  SOME(x) => t :: x :: (eval t)
                  | NONE => [])

	 
end
