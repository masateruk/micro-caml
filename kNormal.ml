(* give names to intermediate values (K-normalization) *)
(* 変換後のコードをなるべくオリジナルに近いものにするため、実際にはほとんどK正規形にはしない。 *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Nil of Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t 
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
and e =
  | Bool of bool
  | Int of int
  | Not of e
  | Neg of e
  | Add of e * e
  | Sub of e * e
  | Mul of e * e
  | Div of e * e
  | Eq of e * e
  | LE of e * e
  | Var of Id.t
  | App of e * e list
  | ExtFunApp of Id.t * e list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec ocaml_of_exp = function
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | Not(e) -> "not " ^ (ocaml_of_exp e)
  | Neg(e) -> "! " ^ (ocaml_of_exp e)
  | Add(e1, e2) -> (ocaml_of_exp e1) ^ " + " ^ (ocaml_of_exp e2)
  | Sub(e1, e2) -> (ocaml_of_exp e1) ^ " - " ^ (ocaml_of_exp e2)
  | Mul(e1, e2) -> (ocaml_of_exp e1) ^ " * " ^ (ocaml_of_exp e2)
  | Div(e1, e2) -> (ocaml_of_exp e1) ^ " / " ^ (ocaml_of_exp e2)
  | Var(x) -> x
  | Eq(e1, e2) -> (ocaml_of_exp e1) ^ " = " ^ (ocaml_of_exp e2)
  | LE(e1, e2) -> (ocaml_of_exp e1) ^ " <= " ^ (ocaml_of_exp e2) 
  | App(e, args) -> "(" ^ (ocaml_of_exp e) ^ " " ^ (String.concat " " (List.map ocaml_of_exp args)) ^ ")"
  | ExtFunApp(x, args) -> "(" ^ x ^ " " ^ (String.concat " " (List.map ocaml_of_exp args)) ^ ")"
    
let rec ocaml_of_expr = function
  | Unit -> "()"
  | Nil _ -> "[]"
  | Exp(e) -> ocaml_of_exp e
  | Cons _ -> assert false
  | If(e, e1, e2) -> "if " ^ (ocaml_of_exp e) ^ "\n\tthen " ^ (ocaml_of_expr e1) ^ "\n\telse " ^ (ocaml_of_expr e2)
  | Let((s1, t), e1, e2) -> "\nlet " ^ s1 ^ " : " ^ (Id.ocaml_of_typ  t) ^ " = " ^ (ocaml_of_expr e1) ^ " in\n" ^ (ocaml_of_expr e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Id.ocaml_of_typ  t) ^ " =\n"
      ^ (ocaml_of_expr e1) ^ " in\n" ^ (ocaml_of_expr e2)

let rec insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Exp(e) -> k e
  | LetRec(fundef, e) ->
      LetRec(fundef, insert_let (e, t) k)
  | _ ->
      let x = Id.gentmp t in
	Let((x, t), e, k (Var(x)))

let rec g env e = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  let _ = D.printf "kNormal.g %s\n" (Syntax.string_of_exp e) in  
  let binop e1 e2 f =
    insert_let (g env e1)
      (fun e1' -> insert_let (g env e2)
	(fun e2' -> f e1' e2')) in
    match e with
  | Syntax.Unit -> Unit, Type.App(Type.Unit, [])
  | Syntax.Nil(t) -> Nil(t), t
  | Syntax.Int(i) -> Exp(Int(i)), Type.App(Type.Int, [])
  | Syntax.Bool(b) -> Exp(Bool(b)), Type.App(Type.Bool, [])
  | Syntax.Not(e) -> insert_let (g env e) (fun e -> Exp(Not(e))), Type.App(Type.Bool, [])
  | Syntax.Neg(e) -> insert_let (g env e) (fun e -> Exp(Neg(e))), Type.App(Type.Int, [])
  | Syntax.Add(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Add(e1', e2'))), Type.App(Type.Int, []) (* 足し算のK正規化 (caml2html: knormal_add) *)
  | Syntax.Sub(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Sub(e1', e2'))), Type.App(Type.Int, []) 
  | Syntax.Mul(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Mul(e1', e2'))), Type.App(Type.Int, []) 
  | Syntax.Div(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Div(e1', e2'))), Type.App(Type.Int, []) 
  | Syntax.Cons(e1, e2) -> assert false
  | Syntax.Var(x) -> Exp(Var(x)), M.find x env
  | Syntax.Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Eq(e1', e2'))), Type.App(Type.Bool, [])
  | Syntax.LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(LE(e1', e2'))), Type.App(Type.Bool, [])
  | Syntax.If(e1, e2, e3) -> 
      let e1', t1 = g env e1 in
      let e2', t2 = g env e2 in
      let e3', t3 = g env e3 in
	insert_let (e1', t1) (fun x -> If(x, e2', e3')), t3
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
	Let((x, t), e1', e2'), t2
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
	LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
	 | Type.App(Type.Arrow, ts) ->
	     let t = List.hd (List.rev ts) in
	     let rec bind xs = function (* "xs" are identifiers for the arguments *)
	       | [] -> Exp(ExtFunApp(f, xs))
	       | e2 :: e2s ->
		   insert_let (g env e2)
		     (fun x -> bind (xs @ [x]) e2s) in
	       (bind [] e2s), t (* left-to-right evaluation *)
	 | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
	| _, Type.App(Type.Arrow, ts) as g_e1 ->
	    let t = List.hd (List.rev ts) in
	      insert_let g_e1
		(fun f ->
		  let rec bind xs = function (* "xs" are identifiers for the arguments *)
		    | [] -> Exp(App(f, xs))
		    | e2 :: e2s ->
			insert_let (g env e2)
			  (fun x -> bind (xs @ [x]) e2s) in
		    bind [] e2s), t (* left-to-right evaluation *)
	| _, t -> Printf.eprintf "type is %s\n" (Type.string_of_typ t); assert false)
	
let f e = 
  let e' = fst (g M.empty e) in
    e'
