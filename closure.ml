type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* ���������Ѵ���μ� (caml2html: closure_t) *)
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
  | Let((s1, t), e1, e2) -> "let " ^ s1 ^ (* " : " ^ (Id.string_of_typ t) ^ *) " = " ^ (string_of_exp e1) ^ " in\n" ^ (string_of_exp e2)
  | Var(s) -> s
  | MakeCls((x, t), { entry = Id.L(l); actual_fv = ys }, e) -> "let " ^ x ^ " : closure = make_closure " ^ l ^ " " ^ (String.concat ", " ys) ^ " in " ^ (string_of_exp e)
  | AppCls(x, args) -> "apply_closure " ^ x ^ " " ^ (String.concat " " args)
  | AppDir(Id.L(x), args) -> x ^ " " ^ (String.concat " " args)

let string_of_fundef { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) (yts @ zts))) ^ (* " : " ^ (Id.string_of_typ t) ^ *) " = " ^ (string_of_exp e) 

let string_of_prog (Prog(defs, e)) = 
  "let rec apply_closure (body, fv) arg = body arg fv\n" ^
  (String.concat "\n" ((List.map string_of_fundef) defs)) ^ "\n(******)\n" ^ (string_of_exp e)
    
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

let rec g env known = function (* ���������Ѵ��롼�������� (caml2html: closure_g) *)
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
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* �ؿ�����ξ�� (caml2html: closure_letrec) *)
      (* �ؿ����let rec x y1 ... yn = e1 in e2�ξ��ϡ�
	 x�˼�ͳ�ѿ����ʤ�(closure��𤵤�direct�˸ƤӽФ���)
	 �Ȳ��ꤷ��known���ɲä���e1�򥯥������Ѵ����Ƥߤ� *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* �����˼�ͳ�ѿ����ʤ��ä������Ѵ����e1'���ǧ���� *)
      (* ���: e1'��x���Ȥ��ѿ��Ȥ��ƽи��������closure��ɬ��!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml����) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
	if S.is_empty zs then known', e1' else
	(* ���ܤ��ä������(toplevel����)���ᤷ�ơ����������Ѵ�����ľ�� *)
	(Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
	 Format.eprintf "function %s cannot be directly applied in fact@." x;
	 toplevel := toplevel_backup;
	 let e1' = g (M.add_list yts env') known e1 in
	 known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* ��ͳ�ѿ��Υꥹ�� *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* �����Ǽ�ͳ�ѿ�z�η����������˰���env��ɬ�� *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* �ȥåץ�٥�ؿ����ɲ� *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') && (zs <> []) then (* x���ѿ��Ȥ���e2'�˽и����뤫����ͳ�ѿ����ʤ��Ȥ��ϴؿ��Τޤ޻��Ѥ��� *)
	MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* �и����Ƥ����������ʤ� *)
      else
	(Format.eprintf "eliminating closure(s) %s@." x;
	 e2') (* �и����ʤ����MakeCls���� *)
  | KNormal.App(x, ys) when S.mem x known -> (* �ؿ�Ŭ�Ѥξ�� (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(c, xs) -> 
      AppCls(c, xs)
  | KNormal.ExtFunApp(x, ys) -> 
      AppDir(Id.L(x), ys)
      
let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
