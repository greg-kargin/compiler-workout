(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

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

    (* Expression evaluator

          val eval : state -> t -> int

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
        "+"  -> ( + )
      | "-"  -> ( - )
      | "*"  -> ( * )
      | "/"  -> ( / )
      | "%"  -> ( mod )
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

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval c st =
      let (s, i, o) = c in
      match st with
        Read x  -> let i_head = List.hd i in
                   let i_tail = List.tl i in
                   let s' = (Expr.update x i_head s) in
                   (s', i_tail, o)
      | Write e -> let r = Expr.eval s e in
                   let o' = (o @ [r]) in
                   (s, i, o')
      | Assign (x, e) -> let r = Expr.eval s e in
                        let s' = (Expr.update x r s) in
                        (s', i, o)
      | Seq (st1, st2) -> let c' = eval c st1 in
                         eval c' st2
  end
