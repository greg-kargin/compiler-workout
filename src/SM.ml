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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
 *)

let rec eval env ((cstack, stack, ((st, i, output) as c)) as conf) = function
  | [] -> conf
  | insn::rest ->
     match insn with
     | BINOP op ->
        let (y::x::stack') = stack in
        eval env (cstack, (Expr.to_func op x y::stack'), c) rest

     | READ ->
        let (z::i') = i in
        eval env (cstack , z::stack, (st, i', output)) rest

     | WRITE ->
        let (z::stack') = stack in
        eval env (cstack, stack', (st, i, output @ [z])) rest

     | CONST i ->
        eval env (cstack, i::stack, c) rest

     | LD x ->
        eval env (cstack, (State.eval st x)::stack, c) rest

     | ST x ->
        let (z::stack') = stack in
        eval env (cstack, stack', (State.update x z st, i, output)) rest

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
        eval env (cstack, stack', c) prg
     | CALL l -> eval env (((rest, st)::cstack), stack, c) (env#labeled l)
     | BEGIN (fun_params, fun_locals) ->
        let assign_val = fun x ((v :: stack), st) -> (stack, State.update x v st) in
        let (stack', st') = List.fold_right assign_val fun_params (stack, State.enter st (fun_params @ fun_locals)) in
        eval env (cstack, stack', (st', i, output)) rest
     | END ->
        begin
          match cstack with
          | (prog, st') :: cs_tail -> eval env (cs_tail, stack, (State.leave st st', i, output)) prog
          | [] -> conf
        end

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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

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
  | Stmt.Repeat (s, e) -> let label = unique_label#get in
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
  | Stmt.Call (fun_name, fun_args) -> List.concat (List.map compile_expr (List.rev fun_args)) @ [CALL fun_name]
  | Stmt.Return opt_res -> (
    match opt_res with
    | Some res -> (compile_expr res) @ [END]
    | _ -> [END]
  )

let rec compile_def (fun_name, (fun_params, fun_locals, fun_body)) =
   [LABEL fun_name; BEGIN (fun_params, fun_locals)] @ compile fun_body @ [END]

 let compile (defs, p) =
   [LABEL "main"] @ compile p @ [END] @ List.concat (List.map compile_def defs)
