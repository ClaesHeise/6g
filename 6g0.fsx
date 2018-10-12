(* Opgave 6g.0
 *
 * Gruppe 10:
 *  Jonas Friis
 *  Claes Christian Heise
 *  Morten Risum Pedersen
 *)

///<summary>
///Takes a list of integers as a continued fraction,
///and computes the comparative real number.
///</summary>
///<params name="lst">
///A list of integers, where each int qn = q0+(1/(q1+(1/(q2+...))))
///</params>
///<returns>
///A float
///</returns>
///<remarks>
///Does not take empty lists.
///Does not take lists with more than 16069 elements.
///Does not take integers greater than 2147483647.
///</remarks>
let rec cfrac2float (lst : int list) : float =
  if lst.Length = 1 then float lst.Head
  else float lst.Head + 1.0/(cfrac2float lst.Tail)

let testCases = [
  ("Liste med et element",
    [([0],0.0);
     ([1],1.0);])
  ("Liste med flere elementer",
    [([0;2],0.5);
     ([1;2],1.5);
     ([1;2;3],1.42857);
     ([1;2;3;4],1.43333);
     ([3;4;12;4],3.245);])
  ("Liste med negative tal",
    [([-100;1],-99.0);
     ([-100;2],-99.5);
     ([-100;-1],-101.0);])
  ("Liste med specialtal (division med 0, negative tal, int32 tal",
    [([9;0],infinity);
     ([9;1],10.0);
     ([9;-1],8.0);
     ([9;-2],8.5);
     ([9;System.Int32.MaxValue],9.0);])
  ]

printfn "\nBlack-box testing of cfrac2float"
for i = 0 to testCases.Length - 1 do
  let (setName, testSet) = testCases.[i]
  printfn "  %d. %s" (i+1) setName
  for j = 0 to testSet.Length - 1 do
    let (lst, expected) = testSet.[j]
    let output = System.Math.Round(cfrac2float lst, 5)
    printfn "   Test %d - %b" (j+1) (output = expected)