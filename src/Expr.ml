(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* The type for the expression. Note, in regular OCaml there is no "@type..."
   notation, it came from GT.
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

(* Available binary operators:
    !!                   --- disjunction
    &&                   --- conjunction
    ==, !=, <=, <, >=, > --- comparisons
    +, -                 --- addition, subtraction
    *, /, %              --- multiplication, division, reminder
*)

(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* An example of a non-trivial state: *)
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution.
let _ =
  List.iter
    (fun x ->
       try  Printf.printf "%s=%d\n" x @@ s x
       with Failure s -> Printf.printf "%s\n" s
    ) ["x"; "a"; "y"; "z"; "t"; "b"]
 *)

(* Expression evaluator

     val eval : state -> expr -> int

   Takes a state and an expression, and returns the value of the expression in
   the given state.
 *)

let lt x y = if x < y then 1 else 0
let gt x y = if x > y then 1 else 0
let nt x   = if x == 0 then 1 else 0
let le x y = nt @@ gt x y
let ge x y = nt @@ lt x y
let eq x y = if x == y then 1 else 0
let neq x y = nt @@ eq x y
let con x y = if x == 0 || y == 0 then 0 else 1
let dis x y = nt @@ con (nt x) (nt y)

let eval_binop op =
  match op with
    "+"  -> fun x y -> x + y
  | "-"  -> fun x y -> x - y
  | "*"  -> fun x y -> x * y
  | "/"  -> fun x y -> x / y
  | "%"  -> fun x y -> x mod y
  | "<"  -> lt
  | ">"  -> gt
  | "<=" -> le
  | ">=" -> ge
  | "==" -> eq
  | "!=" -> neq
  | "&&" -> con
  | "!!" -> dis
  | _    -> failwith "unknown binop"

let rec eval s e =
  match e with
    Const n            -> n
  | Var   v            -> s v
  | Binop (op, e1, e2) -> (eval_binop op) (eval s e1) (eval s e2)
