(* Q.1 *)
let moyenne_ponderee (x1:float) (x2:float) (c1:float) (c2:float) : float =
  (((x1*.c1) +. (x2*.c2))/.(c1 +. c2))

(* Q.2 *)
let max_3 (n1:int) (n2:int) (n3:int) : int =
  if (n1>=n2) && (n1>=n3) then n1
  else if (n2>=n1) && (n2>=n3) then n2
  else n3
 
       
(* Q.3 *)
let variance_3 (x1:float) (x2:float) (x3:float) : float =
  let m:float = (x1+.x2+.x3)/.(3.) in (abs_float(x1-.m) +. abs_float(x2-.m) +. abs_float(x3-.m))/.3.
   
(* Q.4 *)
let fiabilite (v1:int) (v2:int) (v3:int) : int =
  if (v1=v2 && v1=v3) then 3
  else if (v1=v2 || v1=v3 || v2=v3) then 2
  else 0

(* Q.5 *)
let prix_place (age:int) (jour:int) (heure:float) : float =
  if age<=14 then 4.5
  else if age<=26 && jour<=5 then 4.90
  else if heure <= 11.0 && heure>=8.0 then 7.10
  else if age <=26 && jour>5 then 7.90
  else 11.40



(* TP2 *)

(* Nombres premiers *)

(* Q1 *)
let rec less_divider (i:int) (n:int) : int =
  if (i=n) then 0
  else if (n mod i=0) then i
  else (less_divider (i+1) n)

(* Q2 *)
let prime (n:int) : bool =
  if ((less_divider 2 n)!=0) then false
  else true

(* Q3 *)
let rec next_prime (n:int) : int =
  if (prime n) then n
  else (next_prime (n+1))

(* Q4  *)
let rec nth_prime (n:int) : int =
  if n=0 then 2
  else next_prime(nth_prime(n-1)+1)

(* Approximation de la racine carrée *)

let r0 = 1.0
 
(* Q5 *)
let r (a:float) (x:float) : float =
  (x+.(a/.x))/.2.0

(* Q6 *)
let rec sqrt_n (n:int) (a:float) : float =
  if n=0 then r0
  else r(a) (sqrt_n (n-1)(a))

(* Q7 *)
let eq_eps (e:float) (x:float) (y:float) : bool =
  (abs_float(x-.y)<=e)

(* Q8 *)
let sqrt_x (e:float) (a:float) : float =
  let rec loop (s:float) (s':float) : float =
    if eq_eps e s (s') then s'
    else loop (s') (r a s')
  in
  (loop (r0)(r a r0))



(* TP3 *)

(* Tri fusion *)

(* Q1 *)
let rec merge (xs: 'a list) (ys: 'a list) : 'a list =
  match (xs,ys) with
    ([],[])->[]
  |(xs,[])-> xs
  |([],ys)-> ys
  |(x::xxs,y::yys)-> if (y<x) then y::(merge (x::xxs) (yys))
      else x::(merge (xxs) (y::yys))
          
 
(* Q2 *)
let rec split (xs: 'a list) : 'a list * 'a list =
  match xs with
    ([])->([],[])
  |(a::[])-> (a::[],[])
  |a::b::xxs-> let (c1,c2) = (split xxs) in (a::c1,b::c2)

(* Q3 *)
let rec merge_sort (xs: 'a list) : 'a list =
  match xs with
    []->[]
  |x::[]->[x]
  |xs -> let (c1,c2) = (split xs) in (merge (merge_sort c1) (merge_sort c2))
  

(* D'autres ordres *)

(* Q1 *)
let rec merge_gen (cmp: 'a -> 'a -> bool) (xs: 'a list) (ys: 'a list) : 'a list =
  match (xs,ys) with
    ([],ys)->ys
  |(xs,[])-> xs
  |(x::xxs,y::yys)-> if (cmp x y) then x::(merge_gen cmp xxs ys)
      else y::(merge_gen cmp xs yys)

(* Q2 *)
let rec merge_sort_gen (cmp: 'a -> 'a -> bool) (xs: 'a list) : 'a list =
  match xs with
    []->[]
  |a::[]-> [a]
  |xs -> let (x,y) = (split xs)
      in (merge_gen (cmp) (merge_sort_gen cmp x) (merge_sort_gen cmp y))

(* Q3 *)
let sort_weight (xs: (int * int) list) : (int * int) list =
  let cmp ((a,b):(int*int)) ((c,d):(int*int)):bool=(a+b)<=(c+d) in
  merge_sort_gen cmp xs
  
  
  
(* TP4 *)

(* Representation des ensembles finis par des listes sans doublons *)

(* Q1 *)
let rec is_in (e: 'a) (l : 'a list) : bool =
  match l with
    []->false
  | h::t->if (e=h) then true else (is_in e t)

(* Q2 *)
let add_elem (e: 'a) (l : 'a list) : 'a list =
  if (is_in e l) then l
  else e::l

(* Q3 *)
let rec is_subset_rec (l1: 'a list) (l2: 'a list) : bool =
  match l1 with
    []->true
  | x::xs-> if (is_in x l2) then (is_subset_rec xs l2) else false

(* Q4 *)
let is_subset (l1: 'a list) (l2: 'a list) : bool =
  List.for_all(fun x-> (is_in x l2)) l1

(* Q5 *)
let eq_set (l1: 'a list) (l2: 'a list) : bool =
  (is_subset l1 l2) && (is_subset l2 l1)

(* Q6 *)
let rec intersection_rec (l1: 'a list) (l2: 'a list) : 'a list =
  match l1 with
    []-> []
  | x::xs-> if (is_in x l2) then x::(intersection_rec xs l2) else (intersection_rec xs l2)
    (* pourquoi [] a la fin*)
(* Q7 *)
let intersection (l1: 'a list) (l2: 'a list) : 'a list =
  List.filter(fun x-> (is_in x l1)) l2

(* Q8 *)
let rec union_rec (l1: 'a list) (l2: 'a list) : 'a list =
  match l1 with
    []->l2
  | x::xs-> (union_rec xs (add_elem x l2))

(* Q9 *)
let union_left (l1: 'a list) (l2: 'a list) : 'a list =
  List.fold_left(fun e r -> (add_elem r e)) l2 l1

(* Q10 *)
let union_right (l1: 'a list) (l2: 'a list) : 'a list =
  List.fold_right(fun e r -> (add_elem e r)) l2 l1

(* Q11 *)
let make_pairs (x:'a) (l: 'b list) : ('a * 'b) list =
  List.map(fun a-> (x,a)) l

(* Q12 *)
let rec product_rec (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1 with
    []->[]
  | x::xs-> (make_pairs x l2)@(product_rec xs l2)
       

(* Q13 *)
let product (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  List.flatten(List.map(fun a-> make_pairs a l2) l1)
    (* pourquoi l2 puis l1*)
  
(* Q14 *)
let rec powerset_rec (l:'a list) : 'a list list =
  _a definir_

(* Q15 *)
let power (l:'a list) : ('a list) list =
  _a definir_
  
(* TP5 *)
  
(* Q.1 *)
let rec lt_btree (bt:'a btree) (x:'a) : bool =
  match bt with
  |  Empty -> true
  | Node(a,g,d) -> if (a>=x) then false else (lt_btree g x)&&(lt_btree d x)
(* Q.2 *)
let rec ge_btree (bt:'a btree) (x:'a) : bool =
  match bt with
  |  Empty -> true
  | Node(a,g,d) -> if (a<x) then false else (ge_btree g x)&&(ge_btree d x)

(* Q.3 *)
let rec is_abr (bt:'a btree) : bool =
  match bt with
  |  Empty -> true
  | Node(a,g,d) -> (lt_btree g a)&&(ge_btree d a)&&(is_abr g)&&(is_abr d)

(* Q.4 *)
let rec mem (bt:'a btree) (x:'a) : bool =
  match bt with
  |  Empty -> false
  | Node(a,g,d) -> if (x=a) then true else if (x<a) then (mem g x) else (mem d x)
(* Q.5 *)
let rec insert (bt:'a btree) (x:'a) : 'a btree =
  match bt with
  |  Empty -> Node(x, Empty, Empty)
  | Node(a,g,d)-> if (x<a) then Node(a,(insert g x),d) else Node(a,g,(insert d x))

(* Q.6 *)
let rec abr_of_list (l:'a list) : 'a btree =
  match l with
  | [] -> Empty
  | x::xs-> (insert (abr_of_list xs) x)

(* Q.7 *)
let rec list_of_abr (bt:'a btree) : 'a list =
  match bt with
  | Empty -> []
  | Node(a,g,d)-> ((list_of_abr g)@a::(list_of_abr d))

(* Q.8 *)
let abr_sort (l:'a list) : 'a list =
  (list_of_abr(abr_of_list l))
