(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty

let rec subst env = function
  | Type.Var(x) when M.mem x env -> M.find x env
  | Type.Var(x) -> Type.Var(x)
  | Type.App(Type.TyFun(xs, t), ys) -> subst env (subst (M.add_list2 xs ys env) t)
  | Type.App(x, ys) -> Type.App(x, (List.map (subst env) ys))
  | Type.Poly(xs, t) -> 
      let ys = List.map (fun _ -> Type.newtyvar ()) xs in
      let t' = subst (M.add_list2 xs (List.map (fun y -> Type.Var(y)) ys) env) t in
	Type.Poly(ys, subst env t')
  | Type.Meta{ contents = Some(t) } -> subst env t
  | Type.Meta{ contents = None } as t -> t
	  
let rec occur x = function (* occur check (caml2html: typing_occur) *)
  | Type.App(Type.TyFun(_, u), ts) -> occur x u || List.exists (occur x) ts
  | Type.App(_, ts) -> List.exists (occur x) ts
  | Type.Poly(_, t) -> occur x t
  | Type.Meta{ contents = Some(t) } -> occur x t
  | Type.Meta(y) -> x == y
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、メタ型変数への代入をする. 成功したら () を返す. (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.App(Type.Unit, xs), Type.App(Type.Unit, ys) 
  | Type.App(Type.Bool, xs), Type.App(Type.Bool, ys) 
  | Type.App(Type.Int, xs), Type.App(Type.Int, ys) 
  | Type.App(Type.Arrow, xs), Type.App(Type.Arrow, ys) -> List.iter2 unify xs ys
  | Type.App(Type.TyFun(xs, u), ys), t2 -> unify (subst (M.add_list2 xs ys M.empty) u) t2
  | t1, Type.App(Type.TyFun(xs, u), ys) -> unify t1 (subst (M.add_list2 xs ys M.empty) u)
  | Type.Poly(xs, u1), Type.Poly(ys, u2) -> unify u1 (subst (M.add_list2 ys (List.map (fun x -> Type.Var(x)) xs) M.empty) u2)
  | Type.Var(x), Type.Var(y) when x = y -> ()
  | Type.Meta{ contents = Some(t1') }, t2 -> unify t1' t2
  | Type.Meta(x), Type.Meta{ contents = Some(t2') } -> unify t1 t2'
  | Type.Meta(x), Type.Meta(y) when x == y -> ()
  | Type.Meta(x), t2 ->
      if occur x t2 then 
	raise (Unify(t1, t2))
      else 
	x := Some(t2)
  | t1, Type.Meta(y) -> unify t2 t1
  | _, _ -> 
	raise (Unify(t1, t2))

let test_unify =
  assert ((unify (Type.App(Type.Int, [])) (Type.App(Type.Int, []))) = ())

let rec expand = function
  | Type.App(Type.TyFun(xs, u), ys) -> expand (subst (M.add_list2 xs ys M.empty) u)
  | Type.Meta{ contents = Some(t) } -> expand t
  | t -> t

let rec generalize env t = 
  let _ = D.printf "Typing.generalize %s\n" (Type.string_of_typ t) in
  let rec metavars vs = function
    | Type.App(Type.TyFun(_, u), ts) -> List.fold_left metavars (metavars vs u) ts
    | Type.App(_, ts) -> List.fold_left metavars vs ts
    | Type.Poly(_, t) -> metavars vs t
    | Type.Meta{ contents = Some(t') } -> metavars vs t'
    | Type.Meta(x) when M.exists (fun _ t' -> match t' with Type.Meta(y) when x == y -> true | _ -> false) env -> vs
    | Type.Meta(x) -> if (List.memq x vs) then vs else x :: vs
    | _ -> vs in
  let ms = metavars [] t in
  let tyvars = List.map (fun m -> match !m with None -> let var = Type.newtyvar () in m := Some(Type.Var(var)); var | _ -> assert false) ms in
    Type.Poly(tyvars, t)
      
let rec instantiate = function
  | Type.Poly(xs, t) -> 
      subst (M.add_list (List.map (fun x -> (x, Type.Meta(Type.newmetavar ()))) xs) M.empty) t
  | t -> t 
  
(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.App(Type.TyFun(xs, t), ys) -> Type.App(Type.TyFun(xs, deref_typ t), List.map deref_typ ys)
  | Type.App(x, ys) -> Type.App(x, List.map deref_typ ys)
  | Type.Poly(xs, t) -> Type.Poly(xs, deref_typ t)
  | Type.Meta({ contents = None } as r) ->
      Printf.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.App(Type.Int, []));
      Type.App(Type.Int, [])
  | Type.Meta({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_term = function
  | Not(e) -> Not(deref_term e)
  | Neg(e) -> Neg(deref_term e)
  | Add(e1, e2) -> Add(deref_term e1, deref_term e2)
  | Sub(e1, e2) -> Sub(deref_term e1, deref_term e2)
  | Eq(e1, e2) -> Eq(deref_term e1, deref_term e2)
  | LE(e1, e2) -> LE(deref_term e1, deref_term e2)
  | Mul(e1, e2) -> Mul(deref_term e1, deref_term e2)
  | Div(e1, e2) -> Div(deref_term e1, deref_term e2)
  | Cons(e1, e2) -> Cons(deref_term e1, deref_term e2)
  | If(e1, e2, e3) -> If(deref_term e1, deref_term e2, deref_term e3)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_typ xt;
	       args = List.map deref_id_typ yts;
	       body = deref_term e1 },
	     deref_term e2)
  | App(e, es) -> App(deref_term e, List.map deref_term es)
  | e -> e

let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  let _ = D.printf "Typing.g %s\n" (string_of_exp e) in
  try
    match e with
    | Unit -> Type.App(Type.Unit, [])
    | Nil _ -> assert false (* not implemented *)
    | Bool(_) -> Type.App(Type.Bool, [])
    | Int(_) -> Type.App(Type.Int, [])
    | Not(e) ->
	unify (Type.App(Type.Bool, [])) (g env e);
	Type.App(Type.Bool, [])
    | Neg(e) ->
	unify (Type.App(Type.Int, [])) (g env e);
	Type.App(Type.Int, [])
    | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
	unify (Type.App(Type.Int, [])) (g env e1);
	unify (Type.App(Type.Int, [])) (g env e2);
	Type.App(Type.Int, [])
    | Cons _ -> assert false (* not implemented *)
(*	  
    | Cons(e1, e2) ->
	let t1 = g env e1 in
	let t2 = g env e2 in
	  (match t2 with
	    | Type.List(t2') -> 
		let _ = unify t1 t2' in
		  Type.List(t2')
	    | _ -> raise (Unify(t1, t2)))
*)	  
    | Eq(e1, e2) | LE(e1, e2) ->
	unify (g env e1) (g env e2);
	Type.App(Type.Bool, [])
    | If(e1, e2, e3) ->
	unify (g env e1) (Type.App(Type.Bool, []));
	let t2 = g env e2 in
	let t3 = g env e3 in
	  unify t2 t3;
	  t2
    | Let((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
	let t1 = g env e1 in
	let t1' = generalize env t1 in (* 副作用は未サポートなので、Tiger本のp.335にある代入の判定はなし *)
	  unify t t1';
	  g (M.add x t1' env) e2
    | Var(x) when M.mem x env -> instantiate (M.find x env) (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> instantiate (M.find x !extenv)
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
	Format.eprintf "free variable %s assumed as external@." x;
	let t = Type.Meta(Type.newmetavar ()) in
	  extenv := M.add x t !extenv;
	  instantiate t
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
	let t2 = Type.Meta(Type.newmetavar()) in
	let t' = Type.App(Type.Arrow, ((List.map snd yts) @ [t2])) in
	let t1 = g (M.add_list yts (M.add x t' env)) e1 in
	  unify t t';
	  unify t2 t1;
	  let t'' = generalize env t' in
	    g (M.add x t'' env) e2
    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
	let e' = g env e in
	let es' = List.map (g env) es in
	let result = Type.Meta(Type.newmetavar ()) in
	  unify e' (Type.App(Type.Arrow, es' @ [result]));
	    result
  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2))

let f e = 
  extenv := M.empty;
  (try unify (Type.App(Type.Unit, [])) (g M.empty e)
    with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  deref_term e

