(** Signature of module providing arbitrary precision arithmetic on natural numbers *)
module type BigNatType =
  sig
    type bignat

    (** The number 0 **)
    val zero: bignat

    (** The number 1 **)
    val one: bignat

    (** Convert an int to a big natural *)
    val from_int : int -> bignat

    (** Convert a big natural to an int (may overflow) *)
    val to_int : bignat -> int

    (** Convert a big naturals to a string in decimal representation *)
    val to_string : bignat -> string

    (** Addition of big naturals *)
    val (+) : bignat -> bignat -> bignat

    (** Subtraction of big naturals. Raises [Invalid_argument] if result is negative. *)
    val (-) : bignat -> bignat -> bignat

    (** Multiplication of big naturals *)        
    val ( * ) : bignat -> bignat -> bignat

    (** Division of big naturals *)        
    val (/) : bignat -> bignat -> bignat

    (** Modulus of big naturals *)            
    val (mod) : bignat -> bignat -> bignat

    (** Exponentiation of big naturals *)            
    val ( ** ) : bignat -> bignat -> bignat

    (** Compare whether one big natural is less that or equal to another *)            
    val ( <= ) : bignat -> bignat -> bool

    (** Compare whether one big natural is less than another *)            
    val ( < ) : bignat -> bignat -> bool

    (** Check whether a big natural is even *)
    val is_even : bignat -> bool
  end


module BigNat : BigNatType =
  struct
    type bignat = int list (* least significant first *)

    let base = 10000 (* must satisfy: float_of_int base <= sqrt (float_of_int max_int) *)

    let zero = []

    let one = [1]
    
    let rec from_int = function
      | 0 -> []
      | n ->
          if n < 0 then raise (Invalid_argument "negative number")
          else n mod base :: from_int (n / base)

    let rec to_int = function
      | [] -> 0
      | d :: ds -> d + base * to_int ds	(* may overflow *)

    let rec pad =
      let base_len = String.length (string_of_int base) - 1 in
      fun s -> if String.length s = base_len then s else pad ("0" ^ s)

    let to_string ds =
      match List.rev ds with
      | [] -> "0"
      | d :: dr ->
          string_of_int d :: List.map (fun d -> d |> string_of_int |> pad) dr |>
          String.concat ""

    (* like :: on type int list but drops leading 0s *)
    let zcons d ds =
      match d, ds with
      | 0, []-> []
      | _ -> d :: ds

    let rec is_even = function
      | [] -> true
      | d :: dr -> d mod 2 = 0
            
    let rec add ar br c =
      match ar, br, c with
      | ar, [], 0 -> ar
      | [], br, 0 -> br
      | [], [], c -> [c]
      | ar, [], c -> add ar [0] c
      | [], br, c -> add [0] br c
      | a :: ar', b :: br', c ->
          let d, c' =
            if a + b + c < base
            then a + b + c, 0    
            else a + b + c - base, 1      
          in
          d :: add ar' br' c'
                
    let rec sub ar br c =
      match ar, br, c with
      | _, [], 0 -> ar
      | [], br, c -> raise (Invalid_argument "result is negative")
      | ar, [], c -> sub ar [0] c
      | a :: ar', b :: br', c ->
          let d, c' =
            if a - b - c >= 0
            then a - b - c, 0
            else a - b - c + base, 1
          in
          zcons d (sub ar' br' c')
            (*failwith "not implemented" (* INSERT CODE HERE *)*)
            
    let (+) x y = add x y 0
    let (-) x y = sub x y 0


    let rec mul ar b =
      match ar, b with
      | _, 0 
      | [], _ -> []
      | a :: ar, b ->
          from_int (a * b) + (0 :: mul ar b)

    let rec ( * ) x br =
      match x, br with
      | [], _ 
      | _, [] -> []
      | x, b :: br ->
          mul x b + (0 :: (x * br))

    let (<=) x y =
      try
        ignore (sub y x 0);
        true
      with Invalid_argument _ -> false

    let (<) x y = x <> y && x <= y
        
    let rec divmod x y =
      match x, y with
      | _, [] -> raise Division_by_zero
      | x, y ->
          if x < y
          then [], x
          else
	    let rec dm x y c =
              if x < y then x, c
	      else dm (x - y) y Pervasives.(c + 1)
            in
            let d, m = divmod x (0 :: y) in
            let m', c = dm m y 0 in
            zcons c d, m'

    let (/) x y = divmod x y |> fst
    let (mod) x y = divmod x y |> snd

    let rec ( ** ) x y =
      match y with
      | [] -> [1]
      | _ -> x * (x ** (y - [1]))
            (* failwith "not yet implemented" (* INSERT CODE HERE *)*)
  end
   
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

    
(** Signature of module providing arbitrary precision arithmetic on integers *)
module type BigIntType =
  sig
    type bigint

    (** The number 0 *)
    val zero : bigint
        
    (** The number 1 *)
    val one : bigint
          
    (** Convert an int to a big integer *)
    val from_int : int -> bigint

    (** Convert a big integer to an int (may overflow) *) 
    val to_int : bigint -> int

    (** Convert a big integer to a string in decimal representation *)
    val to_string : bigint -> string

    (** Negation of a big integer *)
    val neg : bigint -> bigint

    (** Addition of big integers *)
    val (+) : bigint -> bigint -> bigint

    (** Subtraction of big integers *)
    val (-) : bigint -> bigint -> bigint

    (** Multiplication of big integers *)
    val ( * ) : bigint -> bigint -> bigint

    (** Division of big integers *)
    val (/) : bigint -> bigint -> bigint

    (** Modulus of big integers *)
    val (mod) : bigint -> bigint -> bigint

    (** Exponentiation of big integers. Raises [Invalid_argument] if exponent is negative *)
    val ( ** ) : bigint -> bigint -> bigint

    (** Compare whether one big natural is less than or equal to another *)        
    val ( <= ) : bigint -> bigint -> bool

    (** Compare whether one big natural is less than another *) 
    val ( < ) : bigint -> bigint -> bool

    (** Absolute value of a big integer *)
    val abs : bigint -> bigint

    (** Check whether a big natural is even *)
    val is_even : bigint -> bool

    (** Check whether a big integer is negative *)
    val is_negative : bigint -> bool
  end

      
module MakeBigInt(BigNat : BigNatType) : BigIntType =
  struct
    type bigint = bool * BigNat.bignat

    let zero = true, BigNat.zero
    let one = true, BigNat.one
          
    let from_int x =
      (x >= 0, BigNat.from_int (abs x))

    let to_int (s, n) =
      let i = BigNat.to_int n in
      if s then i else -i

    let to_string (s, n) = (if s then "" else "-") ^ BigNat.to_string n
			  
    let (<=) m n =
      match m, n with
      | (true, _), (false, _) -> false
      | (false, _), (true, _) -> true
      | (true, n1), (true, n2) -> BigNat.(n1 <= n2)
      | (false, n1), (false, n2) -> BigNat.(n2 <= n1)

    let (<) m n =
      match m, n with
      | (true, _), (false, _) -> false
      | (false, _), (true, _) -> true
      | (true, n1), (true, n2) -> BigNat.(n1 < n2)
      | (false, n1), (false, n2) -> BigNat.(n2 < n1)

    (* normalize representation of 0 *)
    let norm (_, n as x) = 
	if n = BigNat.from_int 0 then (true, n) else x

    let neg (s, n) = norm (not s, n)

    let (+) (s1, n1) (s2, n2) =
      if s1 = s2 then
        (s1, BigNat.(n1 + n2))
      else
        try
	  norm (s1, BigNat.(n1 - n2)) 
	with Invalid_argument _ ->
          norm (s2, BigNat.(n2 - n1))
	
    let (-) z1 z2 = z1 + neg z2

    let ( * ) (s1, n1) (s2, n2) = 
      norm (s1 = s2, BigNat.(n1 * n2))

    let divmod (s1, n1) (s2, n2) =
      let d = norm (s1 = s2, BigNat.(n1 / n2)) in
      let m = BigNat.(n1 mod n2) in
      (d, norm (s1, m))
	

    let (/) z1 z2 = divmod z1 z2 |> fst

    let (mod) z1 z2 = divmod z1 z2 |> snd
	
    let ( ** ) z1 z2 =
      match z1, z2 with
      | _, (false, _) -> raise (Invalid_argument "negative exponent")
      | (s, n), (_, m) -> (s || BigNat.is_even m, BigNat.(n ** m))
 
    let abs (_, n) = (true, n)
  
    let is_even (_, n) = BigNat.is_even n
			
    let is_negative (s, _) = not s
  end

module BigInt = MakeBigInt(BigNat)
