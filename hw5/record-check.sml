structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun check(t)= (case t of
                  L.Record((l1 , v1) :: nil) => (case (l1, v1) of 
                                                   (lbl, L.Record(a :: b)) => L.Record( (lbl, check(L.Record(a :: b))) :: nil )
                                                  | _ => L.Record((l1, v1) :: nil)
                                                ) 

                | L.Record((l1 , v1) :: (l2 , v2) :: t) => if l1 = l2
                                                   then 
                                                   raise Fail "Inputted record has two matching labels!"
                                                   else 
                                                   (case ( check(L.Record((l1, v1) :: t)) , check(L.Record((l2, v2) :: t)) ) of
                                                    (L.Record((l1, v1) :: t1), L.Record((l2, v2) :: t2)) => L.Record((l1, v1) :: (l2, v2) :: t1)
                                                    | _ => raise Fail "SML is proving nondeterministic..."
                                                   )
                | L.Record(nil) => raise Fail "Inputted record is an empty record!"
                | _ => raise Fail "The term passed into 'check' is not of type record!"
                )


end
