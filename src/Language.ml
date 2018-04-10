(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators

(* States *)
module State =
  struct

    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty = let fail = fun x -> failwith (Printf.sprintf "Undefined variable %s" x) in
                {g = fail; l = fail; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let update_scope s' x = fun y -> if x = y then v else s' y in
      if List.mem x s.scope then
        {g = s.g; l = update_scope s.l x; scope = s.scope}
      else
        {g = update_scope s.g x; l = s.l; scope = s.scope}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if List.mem x s.scope then s.l x else s.g x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {g = st.g; l = empty.l; scope = xs}

    (* Drops a scope *)
    let leave st st' = {g = st.g; l = st'.l; scope = st'.scope}

  end

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

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)

    let rec eval st expr =
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)
    ostap (
      parse:
        !(Ostap.Util.expr
            (fun x -> x)
            (Array.map (fun (a, s) -> a,
                        List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s)
               [|
                 `Lefta, ["!!"];
                 `Lefta, ["&&"];
                 `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
                 `Lefta, ["+" ; "-"];
                 `Lefta, ["*" ; "/"; "%"];
               |])
            primary);
      primary:
        n:DECIMAL {Const n}
        | x:IDENT   {Var x}
    | -"(" parse -")"
    )
  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read        of string
    (* write the value of an expression *) | Write       of Expr.t
    (* assignment                       *) | Assign      of string * Expr.t
    (* composition                      *) | Seq         of t * t
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes a configuration and a statement, and returns another configuration
     *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> (State.update x z st, i', o)
                                       | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval env (eval env conf s1) s2
      | Skip -> conf
      | If (e, s1, s2) -> if Expr.eval st e != 0
                          then eval env conf s1
                          else eval env conf s2
      | While (e, s)   -> if Expr.eval st e != 0
                          then eval env (eval env conf s) stmt
                          else conf
      | Repeat (s, e) -> let ((st', i', o') as conf') = eval env conf s in
                         if Expr.eval st' e = 0
                         then eval env conf' stmt
                         else conf'
      | Call (fun_name, fun_args) ->
         let (fun_params, fun_locals, fun_body) = env#definition fun_name in
         let st' = State.enter st (fun_params @ fun_locals) in
         let assign_vals = fun acc_st param exp ->
           State.update param (Expr.eval st exp) acc_st in
         let fun_st = List.fold_left2 assign_vals st' fun_params fun_args in
         let (res_st, res_i, res_o) = eval env (fun_st, i, o) fun_body in
         ((State.leave res_st st), res_i, res_o)

    ostap (
      parse : seq | stmt;
      stmt  : read | write | assign | skip | if_ | while_ | repeat | for_;
      read  : %"read" -"(" x:IDENT -")" { Read x };
      write : %"write" -"(" e:!(Expr.parse) -")" { Write e };
      assign: x:IDENT -":=" e:!(Expr.parse) { Assign (x, e) };
      seq   : s1:stmt -";" s2:parse { Seq(s1, s2) };
      skip  : %"skip" { Skip };

      if_   :
        %"if" e:!(Expr.parse)
        %"then" s1:parse
           elifs :(%"elif" !(Expr.parse) %"then" parse)*
           else_ :(%"else" parse)? %"fi"
                 {
                   let else_body = match else_ with
                     | Some t -> t
                     | None -> Skip
                   in
                   let else_body' = List.fold_right (fun (e_, t_) t -> If (e_, t_, t)) elifs else_body in
                   If (e, s1, else_body')
              };

      while_ : %"while" e:!(Expr.parse)
               %"do" s:parse
               %"od" { While (e, s) };

      for_   : %"for" s1:parse "," e:!(Expr.parse) "," s2:parse
               %"do" s3:parse
               %"od" { Seq (s1, While (e, Seq (s3, s2)))};

      repeat : %"repeat" s:parse
               %"until" e:!(Expr.parse)
                  { Repeat (s, e) };
      fun_call : fun_name:IDENT -"(" fun_args:!(Expr.parse)* -")"
                    { Call(fun_name, fun_args) }
    )

  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: %"fun" fun_name:IDENT -"(" fun_params:(IDENT)* -")" fun_locals:(%"local" (IDENT)*)? -"{" s:!(Stmt.parse) -"}"
                                                                                                                         { (fun_name, (fun_params, (match fun_locals with None -> [] | Some xs -> xs), s)) }
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m = List.fold_left (fun acc_map (fun_name, fun_def) -> M.add fun_name fun_def acc_map) M.empty defs in
  let (_, _, res_o) = Stmt.eval (object method definition f = M.find f m end) (State.empty, i, []) body in
  res_o

(* Top-level parser *)
ostap (
  parse: !(Definition.parse)* !(Stmt.parse)
)
