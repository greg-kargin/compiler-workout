open GT
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string
(* conditional jump                *) | CJMP  of string * string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
 *)

let rec eval env ((stack, ((st, i, output) as c)) as conf) = function
  | [] -> conf
  | insn::rest ->
     match insn with
     | BINOP op ->
        let (y::x::stack') = stack in
        eval env (Expr.to_func op x y::stack', c) rest

     | READ ->
        let (z::i') = i in
        eval env (z::stack, (st, i', output)) rest

     | WRITE ->
        let (z::stack') = stack in
        eval env (stack', (st, i, output @ [z])) rest

     | CONST i ->
        eval env (i::stack, c) rest

     | LD x ->
        eval env (st x::stack, c) rest

     | ST x ->
        let (z::stack') = stack in
        eval env (stack', (Expr.update x z st, i, output)) rest

     | LABEL l ->
        eval env conf rest

     | JMP label ->
        eval env conf (env#labeled label)

     | CJMP (cond, label) ->
        let (top::stack') = stack in
        let prg = (if (cond = "z" && top = 0) || (cond = "nz" && top != 0)
                   then env#labeled label
                   else rest)
        in
        eval env (stack', c) prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let unique_label = object
    val mutable label = 0
    method get =
      label <- label + 1;
      string_of_int label
  end

let rec compile pr =
  let rec compile_expr = function
    | Expr.Var   x          -> [LD x]
    | Expr.Const n          -> [CONST n]
    | Expr.Binop (op, x, y) -> compile_expr x @ compile_expr y @ [BINOP op]
  in
  match pr with
  | Stmt.Assign (x, e) -> (compile_expr e) @ [ST x]
  | Stmt.Read x -> [READ] @ [ST x]
  | Stmt.Write e -> (compile_expr e) @ [WRITE]
  | Stmt.Seq (s1, s2) -> (compile s1) @ (compile s2)
  | Stmt.Skip -> []
  | Stmt.RepeatUntil (s, e) -> let label = unique_label#get in
                               [LABEL label] @ (compile s) @ (compile_expr e) @ [CJMP ("nz", label)]
  | Stmt.While (e, s) -> let label1 = unique_label#get in
                         let label2 = unique_label#get in
                         [LABEL label1] @ compile_expr e @ [CJMP ("z", label2)] @
                           compile s @ [JMP label1; LABEL label2]
  | Stmt.If (e, s1, s2) -> let label1 = unique_label#get in
                           let label2 = unique_label#get in
                           compile_expr e @ [CJMP ("z", label1)] @
                             compile s1 @ [JMP label2; LABEL label1] @
                               compile s2 @ [LABEL label2]
