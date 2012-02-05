(* give names to intermediate values (K-normalization) *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Nil of Type.t
  | Bool of bool
  | Int of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | Cons of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
	Let((x, t), e, e'), t'

let rec g env e = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  let _ = D.printf "kNormal.g %s\n" (Syntax.string_of_exp e) in  
    match e with
  | Syntax.Unit -> Unit, Type.App(Type.Unit, [])
  | Syntax.Nil(t) -> Nil(t), t
  | Syntax.Bool(b) -> Bool(b), Type.App(Type.Bool, [])
  | Syntax.Int(i) -> Int(i), Type.App(Type.Int, [])
  | Syntax.Not(e) -> 
      insert_let (g env e)
	(fun x -> Not(x), Type.App(Type.Bool, []))
  | Syntax.Neg(e) ->
      insert_let (g env e)
	(fun x -> Neg(x), Type.App(Type.Int, []))
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y -> Add(x, y), Type.App(Type.Int, [])))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y -> Sub(x, y), Type.App(Type.Int, [])))
  | Syntax.Mul(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y -> Mul(x, y), Type.App(Type.Int, [])))
  | Syntax.Div(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y -> Div(x, y), Type.App(Type.Int, [])))
  | Syntax.Cons(e1, e2) ->
      let (e1', t1) = (g env e1) in
      let (e2', t2) = (g env e2) in
	insert_let (e1', t1)
	  (fun x -> insert_let (e2', t2)
	    (fun y -> Cons(x, y), t2))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y ->
	    let e3', t3 = g env e3 in
	    let e4', t4 = g env e4 in
	      IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	  (fun y ->
	    let e3', t3 = g env e3 in
	    let e4', t4 = g env e4 in
	      IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
	Let((x, t), e1', e2'), t2
  | Syntax.Var(x) -> 
      Var(x), M.find x env
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
	      | [] -> ExtFunApp(f, xs), t
	      | e2 :: e2s ->
		  insert_let (g env e2)
		    (fun x -> bind (xs @ [x]) e2s) in
	      bind [] e2s (* left-to-right evaluation *)
	| _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
	| _, Type.App(Type.Arrow, ts) as g_e1 ->
	    let t = List.hd (List.rev ts) in
	    insert_let g_e1
	      (fun f ->
		let rec bind xs = function (* "xs" are identifiers for the arguments *)
		  | [] -> App(f, xs), t
		  | e2 :: e2s ->
		      insert_let (g env e2)
			(fun x -> bind (xs @ [x]) e2s) in
		  bind [] e2s) (* left-to-right evaluation *)
	| _, t -> Printf.eprintf "type is %s\n" (Type.string_of_typ t); assert false)
	
let f e = fst (g M.empty e)
  
