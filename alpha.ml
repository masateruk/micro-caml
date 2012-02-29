(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x
let add x y env = if (M.mem x env) then env else (M.add x y env)
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let genid x env = if (M.mem x env) then Id.genid x else x

let rec h env = function
  | Bool(b) -> Bool(b)
  | Int(i) -> Int(i)
  | Not(e) -> Not(h env e)
  | Var(x) -> Var(find x env)
  | Neg(e) -> Neg(h env e)
  | Add(e1, e2) -> Add(h env e1, h env e2)
  | Sub(e1, e2) -> Sub(h env e1, h env e2)
  | Mul(e1, e2) -> Mul(h env e1, h env e2)
  | Div(e1, e2) -> Div(h env e1, h env e2)
  | Eq(e1, e2) -> Eq(h env e1, h env e2)
  | LE(e1, e2) -> LE(h env e1, h env e2)
  | App(e, ys) -> App(h env e, List.map (h env) ys)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (h env) ys)
    
let rec g env = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Nil(t) -> Nil(t)
  | Exp(e) -> Exp(h env e)
  | Cons(x, y) -> Cons(find x env, find y env)
  | If(e, e1, e2) -> If(h env e, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = genid x env in
      Let((x', t), g env e1, g (add x x' env) e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let env = add x (genid x env) env in
      let ys = List.map fst yts in
      let env' = add_list2 ys (List.map (fun y -> genid y env) ys) env in
      LetRec({ name = (find x env, t);
	       args = List.map (fun (y, t) -> (find y env', t)) yts;
	       body = g env' e1 },
	     g env e2)

let f = g M.empty
