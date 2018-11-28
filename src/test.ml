open OUnit2
open Arithmetic
  
let n = 10000000
let bn = BigNat.from_int n

let fac n =
  let open BigNat in
  let rec fac n acc =
    if n = zero
    then acc
    else fac (n - one) (n * acc)
  in
  fac n one

    
let digits n =
  n |> BigNat.to_string |> String.length 

let test1 test_ctxt =
  assert_equal (BigNat.(bn + bn |> to_int)) (n + n) 

let test2 test_ctxt =
  assert_equal (BigNat.(bn * bn |> to_int)) (n * n) 

let test3 test_ctxt =
  assert_equal (BigNat.(bn - bn |> to_int)) 0 

let test4 test_ctxt =
  assert_raises
    (Invalid_argument "result is negative")
    (fun () -> BigNat.(bn - one - bn |> to_int))
    
let test5 test_ctxt =
  assert_equal (BigNat.(bn * bn / bn)) bn

let test6 test_ctxt =
  assert_equal
    (BigNat.from_int 100 |> fac |> BigNat.to_string)
    "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"

let test7 test_ctxt =
  assert_equal (333 |> BigNat.from_int |> fac |> digits) 698
    
let suite =
  "BigNat suite" >:::
  ["test1" >:: test1;
   "test2" >:: test2;
   "test3" >:: test3;
   "test4" >:: test4;
   "test5" >:: test5;
   "test6" >:: test6;
   "test7" >:: test7]

let () = run_test_tt_main suite

