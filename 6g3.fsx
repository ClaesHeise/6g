(* Opgave 6g.3
 *
 * Gruppe 10:
 *  Jonas Friis
 *  Claes Christian Heise
 *  Morten Risum Pedersen
 *)

///<summary>
///Takes a continued fraction as an int list and returns an integer fraction
///as a tuple.
///</summary>
///<params name="lst">
///A continued fraction as an int list
///</params>
///<params name="i">
///An index in the continued fraction (with purpose???)
///</params>
///<returns>
///A reduced integer fraction, represented as a tuple (num./den.)
///</returns>
///<remarks>
///
///</remarks>
let rec cfrac2frac (lst : int list) (i : int) : (int * int) =
  if i=0 then (lst.Head,1)
  else (cfrac2frac lst.Tail (i-1))

printfn "%A" (cfrac2frac [1;2;3] 2)

//Returnerer en frac på sidste element;
//få den til også at gøre det op ad listen!