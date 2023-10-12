structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next l =
	         
      	       (case l
      	      	of  #"Z" :: t => SOME (T.Z , t)
	        | #"[" :: t => SOME (T.LBrack, t)
		| #"]" :: t => SOME (T.RBrack, t)
		| #"+" :: t => SOME (T.Plus, t)
		| #"-" :: t => SOME (T.Minus, t)
		| #"<" :: t => SOME (T.LessThan, t)
		| #">" :: t => SOME (T.GreaterThan, t)
		| #"?" :: t => SOME (T.QuestionMark, t)
		| #":" :: t => SOME (T.Colon, t)
		| #"T" :: t => SOME (T.T , t)
		| #"F" :: t => SOME (T.F, t)
		| #"S" :: t => SOME (T.S, t)
		| #"P" :: t => SOME (T.P, t)
		| #"&" :: #"&" :: t => SOME (T.DoubleAmpersand, t)
		| #"|" :: #"|" :: t => SOME (T.DoublePipe, t)
		| [] => NONE
		| h :: t => if (Char.isSpace h) then (next t) else raise Fail "Bad Token!")
		
  fun scan(s) = let
	      	   val l = explode s
		   val t = next l
		in
                   (case t
		    of NONE => []
		    | SOME (a, b) => a :: (scan (implode b)))
		end
	    
  
end      
      

