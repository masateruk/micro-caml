type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
type fundef = { name : Id.l * Type.t;
		args : (Id.t * Type.t) list;
		formal_fv : (Id.t * Type.t) list;
		body : t }
type prog = Prog of fundef list * t

let rec string_of_exp = function
  | Unit -> "()"
  | Nil _ -> "[]"
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | Not(s) -> "not " ^ s
  | Neg(s) -> "! " ^ s
  | Add(s1, s2) -> s1 ^ " + " ^ s2
  | Sub(s1, s2) -> s1 ^ " - " ^ s2
  | Mul(s1, s2) -> s1 ^ " * " ^ s2
  | Div(s1, s2) -> s1 ^ " / " ^ s2
  | Cons(s1, s2) -> s1 ^ " :: " ^ s2
  | IfEq(s1, s2, e1, e2) -> "if " ^ s1 ^ " = " ^ s2 ^ "\n\tthen " ^ (string_of_exp e1) ^ "\n\telse " ^ (string_of_exp e2)
  | IfLE(s1, s2, e1, e2) -> "if " ^ s1 ^ " < " ^ s2 ^ "\n\tthen " ^ (string_of_exp e1) ^ "\n\telse " ^ (string_of_exp e2)
  | Let((s1, t), e1, e2) -> "let " ^ s1 ^ " : " ^ (Id.ocaml_of_typ t) ^ " = " ^ (string_of_exp e1) ^ " in\n" ^ (string_of_exp e2)
  | Var(s) -> s
  | MakeCls((x, t), { entry = Id.L(l); actual_fv = ys }, e) -> "let " ^ x ^ " : closure = make_closure " ^ l ^ " " ^ (String.concat ", " ys) ^ " in " ^ (string_of_exp e)
  | AppCls(x, args) -> "apply_closure " ^ x ^ " " ^ (String.concat " " args)
  | AppDir(Id.L(x), args) -> x ^ " " ^ (String.concat " " args)

let string_of_fundef { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) (yts @ zts))) ^ (* " : " ^ (Id.string_of_typ t) ^ *) " = " ^ (string_of_exp e) 

let rec fv = function
  | Unit | Nil(_) | Bool(_) | Int(_) -> S.empty
  | Not(x) | Neg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | Cons(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) -> S.of_list xs

let toplevel : fundef list ref = ref []

let rec g env known e = (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  let () = D.printf "Closure.g %s\n" (KNormal.ocaml_of_expr e) in
  match e with 
  | KNormal.Unit -> Unit
  | KNormal.Nil(t) -> Nil(t)
  | KNormal.Bool(b) -> Bool(b)
  | KNormal.Int(i) -> Int(i)
  | KNormal.Not(x) -> Not(x)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Div(x, y) -> Div(x, y)
  | KNormal.Cons(x, y) -> assert false (* not implemented *)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      let _ = if x = "fwa30" then D.printf "Begin LetRec ****\n%s\n**** End of LetRec\n" (KNormal.ocaml_of_expr e) else () in
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	 xに自由変数がない(closureを介さずdirectに呼び出せる)
	 と仮定し、knownに追加してe1をクロージャ変換してみる *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let _ = D.printf "M.add %s t env\n" x in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
      let zs = S.diff (fv e1') (S.of_list ((List.map fst yts) @ (List.map (fun { name = (Id.L(x), _); _ } -> x) !toplevel))) in
      let known', e1' =
	if S.is_empty zs then known', e1' else
	(* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
	(D.printf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
	 D.printf "function %s cannot be directly applied in fact@." x;
	 toplevel := toplevel_backup;
	 let e1' = g (M.add_list yts env') known e1 in
	 known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list ((List.map fst yts) @ (List.map (fun { name = (Id.L(x), _); _ } -> x) !toplevel))))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') && (zs <> []) then (* xが変数としてe2'に出現するか。自由変数がないときは関数のまま使用する *)
	MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
	(D.printf "eliminating closure(s) %s@." x;
	 e2') (* 出現しなければMakeClsを削除 *)
  | KNormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      D.printf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(c, xs) -> 
      AppCls(c, xs)
  | KNormal.ExtFunApp(x, ys) -> 
      AppDir(Id.L(x), ys)
      
let f e =
  let _ = D.printf "Begin of Closure.f %s\n End of Closure.f\n" (KNormal.ocaml_of_expr e) in
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
