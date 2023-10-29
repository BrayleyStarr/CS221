structure Unroll : sig

  val unroll :  Sweetl.prog  -> Sweetl.term
  val contains_abbr : Sweetl.term -> bool
  val update_term : string * Sweetl.term * Sweetl.term -> Sweetl.term
  val prog_sort : string * Sweetl.term * (string * Sweetl.term) list -> (string * Sweetl.term) list
  val find_term : (string * Sweetl.term) list * string -> Sweetl.term
  val complete_term :  (string * Sweetl.term) list * Sweetl.term  -> Sweetl.term 

end = struct

  (* tells you whether or not a given term is an abbreviation or has a sub abbreviation *)
  fun contains_abbr(trm) = (case trm of
                              Sweetl.Abbr(s) => true
                            | Sweetl.App(trm1, trm2) => contains_abbr(trm1) orelse contains_abbr(trm2)
                            | Sweetl.Lam(s, trm) => contains_abbr(trm)
                            | _ => false)

  (*updates any instances of abbr in term with associated abbrt *)
  fun update_term(abbr, at, t) = (case t of
                                         Sweetl.Lam(str, trm) => Sweetl.Lam(str, update_term(abbr, at, trm))
                                       | Sweetl.App(trm1, trm2) => Sweetl.App(update_term(abbr, at, trm1), update_term(abbr, at, trm2 ))
                                       | Sweetl.Abbr(s) => if String.compare(abbr, s) = EQUAL then at else Sweetl.Abbr(s)
                                       | _ => t) 

  (*takes in a tuple list and abbr and the term for the abbr
    and fills in all instances of the abbr in tlist *)
  fun prog_sort(abbr, at, tlst) = (case tlst of
                                    [] => []
                                  | (s, trm) :: t => (s, update_term(abbr, at, trm)) :: prog_sort(abbr, at, t))

  (* takes in a tuple list and makes sure every abbreviation
     is matched to a term that has no sub abbreviations *) 
  fun update_tlst(tlst) = (case tlst of 
                           [] => []
                          | (abbr, abbrt) :: t => (abbr, abbrt) :: update_tlst(prog_sort(abbr, abbrt, t)))

  (*takes in a (string * term) list, an abbr string 
    and returns the term associated with the abbr string from the list *)
  fun find_term(tlst, abbr) = (case tlst of 
                                [] => raise Fail "No term associated with a given abbr!"
                              | (s, trm) :: t => if String.compare(abbr, s) = EQUAL then trm else find_term(t, abbr))

  (* assumes tuple list has no sub abbreviations. Takes a term and fills in every abbr *)
  fun complete_term(prog) = (case prog of 
                                  ([], trm) => trm
                                 | (tlst, trm) => (case trm of
                                          Sweetl.App(Sweetl.Abbr(str1), Sweetl.Abbr(str2)) => Sweetl.App(find_term(tlst, str1), find_term(tlst, str2))
                                         | Sweetl.App(Sweetl.Abbr(str1), trm2) => Sweetl.App(find_term(tlst, str1), complete_term(tlst, trm2))
                                         | Sweetl.App(trm1, Sweetl.Abbr(str2)) => Sweetl.App(complete_term(tlst, trm1), find_term(tlst, str2))
                                         | Sweetl.App(trm1, trm2) =>  Sweetl.App(complete_term(tlst, trm1), complete_term(tlst, trm2))
                                         | Sweetl.Lam(str, Sweetl.Abbr(str2)) => Sweetl.Lam(str, find_term(tlst, str2))
                                         | Sweetl.Lam(str, trm1) => Sweetl.Lam(str, complete_term(tlst, trm1))
                                         | Sweetl.Abbr(str) => find_term(tlst, str)
                                         | _ => trm))

  fun unroll(Sweetl.Prog(tlst, trm): Sweetl.prog): Sweetl.term =
                  (case tlst of
                    [] => trm
                  | (str, abt) :: t => if contains_abbr(abt) then raise Fail "First abbreviation cannot have abbreviations in term"
                                                                    else complete_term((update_tlst((str, abt) :: t) , trm)))

 
                                            
                                  

				 
end
