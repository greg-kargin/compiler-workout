open GT

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)

let exec_insn c p =
  let (st, sc) = c in
  let (s, i, o) = sc in
  match p with
    BINOP op -> (match st with
                   s1 :: s2 :: st' -> let r = (Syntax.Expr.eval_binop op) s1 s2 in
                                      (r :: st, sc)
                 | _ -> failwith "not enough elements on stack")
  | CONST z -> (z :: st, sc)
  | READ -> let i_hd = List.hd i in
            let i_tl = List.tl i in
            (i_hd :: st, (s, i_tl, o))
  | WRITE -> let st_hd = List.hd st in
             let st_tl = List.tl st in
             (st_tl, (s, i, o @ [st_hd]))
  | LD x -> let v = s x in
            (v :: st, sc)
  | ST x -> let st_hd = List.hd st in
            let st_tl = List.tl st in
            (st_tl, ((Syntax.Expr.update x st_hd s), i, o))


(*let eval c p = List.fold_left p c exec_insn*)

let rec eval c p =
  match p with
    ph :: p' -> eval (exec_insn c ph) p'
  | [] -> c


(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec comp_e e =
  match e with
    Syntax.Expr.Const v -> [CONST v]
  | Syntax.Expr.Var x -> [LD x]
  | Syntax.Expr.Binop (op, e1, e2) -> (comp_e e1) @ (comp_e e2) @ [BINOP op]

let rec compile stmt =
  match stmt with
    Syntax.Stmt.Read x -> [READ; ST x]
  | Syntax.Stmt.Write e -> (comp_e e) @ [WRITE]
  | Syntax.Stmt.Assign (x, e) -> (comp_e e) @ [ST x]
  | Syntax.Stmt.Seq (st1, st2) -> (compile st1) @ (compile st2)
