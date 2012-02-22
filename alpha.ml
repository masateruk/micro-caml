(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x
let add x y env = if (M.mem x env) then env else (M.add x y env)
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let genid x env = if (M.mem x env) then Id.genid x else x

let rec g env = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Nil(t) -> Nil(t)
  | Bool(b) -> Bool(b)
  | Int(i) -> Int(i)
  | Not(x) -> Not(find x env)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | Mul(x, y) -> Mul(find x env, find y env)
  | Div(x, y) -> Div(find x env, find y env)
  | Cons(x, y) -> Cons(find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = genid x env in
      Let((x', t), g env e1, g (add x x' env) e2)
  | Var(x) -> Var(find x env)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let env = add x (genid x env) env in
      let ys = List.map fst yts in
      let env' = add_list2 ys (List.map (fun y -> genid y env) ys) env in
      LetRec({ name = (find x env, t);
	       args = List.map (fun (y, t) -> (find y env', t)) yts;
	       body = g env' e1 },
	     g env e2)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

let f = g M.empty
