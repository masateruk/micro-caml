type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* ���������Ѵ���μ� (caml2html: closure_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
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
type typedef = { typ_name : Id.t; typ_def : Type.t }
type prog = Prog of fundef list * t

let rec ml_of_expr = function
  | Unit -> "()"
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | Not(s) -> "not " ^ s
  | Neg(s) -> "! " ^ s
  | Add(s1, s2) -> s1 ^ " + " ^ s2
  | Sub(s1, s2) -> s1 ^ " - " ^ s2
  | Mul(s1, s2) -> s1 ^ " * " ^ s2
  | Div(s1, s2) -> s1 ^ " / " ^ s2
  | IfEq(s1, s2, e1, e2) -> "if " ^ s1 ^ " = " ^ s2 ^ "\n\tthen " ^ (ml_of_expr e1) ^ "\n\telse " ^ (ml_of_expr e2)
  | IfLE(s1, s2, e1, e2) -> "if " ^ s1 ^ " < " ^ s2 ^ "\n\tthen " ^ (ml_of_expr e1) ^ "\n\telse " ^ (ml_of_expr e2)
  | Let((s1, t), e1, e2) -> "let " ^ s1 ^ (* " : " ^ (Id.ml_of_typ t) ^ *) " = " ^ (ml_of_expr e1) ^ " in\n" ^ (ml_of_expr e2)
  | Var(s) -> s
  | MakeCls((x, t), { entry = Id.L(l); actual_fv = ys }, e) -> "((" ^ (ml_of_expr e) ^ "), " ^ (String.concat ", " ys) ^ ")"
  | AppCls(x, args) -> "apply_closure " ^ x ^ " " ^ (String.concat " " args)
  | AppDir(Id.L(x), args) -> x ^ " " ^ (String.concat " " args)

let ml_of_fundef { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) (yts @ zts))) ^ (* " : " ^ (Id.ml_of_typ t) ^ *) " = " ^ (ml_of_expr e) 

let ml_of_prog (Prog(defs, e)) = 
  "let rec apply_closure (body, fv) arg = body arg fv\n" ^
  (String.concat "\n" ((List.map ml_of_fundef) defs)) ^ "\n(******)\n" ^ (ml_of_expr e)
    
let rec fv = function
  | Unit | Bool(_) | Int(_) -> S.empty
  | Not(x) | Neg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) -> S.of_list xs

let toplevel : fundef list ref = ref []

let rec g env known type_check = function (* ���������Ѵ��롼�������� (caml2html: closure_g) *)
  | KNormal.Unit -> Unit, Type.Unit
  | KNormal.Bool(b) -> Bool(b), Type.Bool
  | KNormal.Int(i) -> Int(i), Type.Int
  | KNormal.Not(x) -> Not(x), Type.Bool
  | KNormal.Neg(x) -> Neg(x), Type.Int
  | KNormal.Add(x, y) -> Add(x, y), Type.Int
  | KNormal.Sub(x, y) -> Sub(x, y), Type.Int
  | KNormal.Mul(x, y) -> Mul(x, y), Type.Int
  | KNormal.Div(x, y) -> Div(x, y), Type.Int
  | KNormal.IfEq(x, y, e1, e2) -> 
      let (e1', t1') = g env known type_check e1 in
      let (e2', t2') = g env known type_check e2 in
	(assert(not type_check || Type.eq t1' t2');
	 IfEq(x, y, e1', e2'), t1')
  | KNormal.IfLE(x, y, e1, e2) -> 
      let (e1', t1') = g env known type_check e1 in
      let (e2', t2') = g env known type_check e2 in
	(assert(not type_check || Type.eq t1' t2');
	 IfLE(x, y, e1', e2'), t1')
  | KNormal.Let((x, t), e1, e2) -> 
      let (e1', t1') = g env known type_check e1 in
	(* Typing.unify �Ǥϥ�������� Fun ���ˤʤäƤ���Τǲ���Ƥ����ʳ��� x �η�������ľ����*)
      let (e2', t2') = g (M.add x t env) known type_check e2 in
	Let((x, t1'), e1', e2'), t2'
  | KNormal.Var(x) -> Var(x), (M.find x env)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* �ؿ�����ξ�� (caml2html: closure_letrec) *)
      (* �ؿ����let rec x y1 ... yn = e1 in e2�ξ��ϡ�
	 x�˼�ͳ�ѿ����ʤ�(closure��𤵤�direct�˸ƤӽФ���)
	 �Ȳ��ꤷ��known���ɲä���e1�򥯥������Ѵ����Ƥߤ� *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let (e1', t1') = g (M.add_list yts env') known' false e1 in
	(* �����˼�ͳ�ѿ����ʤ��ä������Ѵ����e1'���ǧ���� *)
	(* ���: e1'��x���Ȥ��ѿ��Ȥ��ƽи��������closure��ɬ��!
           (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml����) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1', t1' =
	if S.is_empty zs then known', e1', t1' else
	  (* ���ܤ��ä������(toplevel����)���ᤷ�ơ����������Ѵ�����ľ�� *)
	  (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
	   Format.eprintf "function %s cannot be directly applied in fact@." x;
	   toplevel := toplevel_backup;
	   let (e1', t1') = g (M.add_list yts env') known type_check e1 in
	     known, e1', t1') in

      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* ��ͳ�ѿ��Υꥹ�� *)
      let zts = List.map (fun z -> (z, M.find z env)) zs in (* �����Ǽ�ͳ�ѿ�z�η����������˰��� env ��ɬ�ס���Ȥ� env' ���ä������ zs �� x ���ޤޤ�ʤ��Τ� env �Ǥ������ *)
	
      (* t �η�̷��� e1' �η����֤������롣��̷��� Fun �ξ��˥���������ѹ����뤿�ᡣ����� Fun �˼�ͳ�ѿ��η����ɲá������Ŭ����ǽ���ɤ���Ƚ���ǽ�ˡ�*)
      let t = match t with Type.Fun(args, _, fv) -> Type.Fun(args, t1', fv @ (List.map snd zts)) | _ -> assert false in 
      let env' = M.add x t env in

      let toplevel_backup = !toplevel in

	toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* �ȥåץ�٥�ؿ����ɲá��ȥåץ�٥�η��Ͼ�� Fun �� *)
	let (e2', t2') = g env' known' false e2 in
	  if S.mem x (fv e2') then (* x���ѿ��Ȥ���e2'�˽и����뤫 *)

	    (* x���ѿ��Ȥ���e2'�˽и�������ϡ�x�򥯥�����Ȥ��ƻ��Ȥ��ʤ��Ȥ����ʤ��Τ� env ��� x �η��򥯥�����ˤ��� e2 ���Ѵ�����ľ�� *)
	    (* e2 �κ��Ѵ�����ʤ������� toplevel ���Ȥ��᤹ *)
	    let _ = toplevel := toplevel_backup in
	    let _ = toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel in
	    let t' = Type.Closure(t, List.map snd zts) in
	    let env' = M.add x t' env in
	    let e2', t2' = g env' known' type_check e2 in
	      
	      (Format.eprintf "making closure(s) %s@." x;
	       MakeCls((x, t'), { entry = Id.L(x); actual_fv = zs }, e2'), t2') (* �и����Ƥ����������ʤ� *)

	  else
	    (Format.eprintf "eliminating closure(s) %s@." x;
	     e2', t2') (* �и����ʤ����MakeCls���� *)
  | KNormal.App(x, ys) when S.mem x known -> (* �ؿ�Ŭ�Ѥξ�� (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys), (Type.apply (M.find x env) (List.map (fun y -> (M.find y env)) ys))
  | KNormal.App(c, xs) -> 
      AppCls(c, xs), (Type.apply (M.find c env) (List.map (fun x -> (M.find x env)) xs))
  | KNormal.ExtFunApp(x, ys) -> 
      AppDir(Id.L(x), ys), (Type.apply (M.find x !Typing.extenv) (List.map (fun y -> (M.find y env)) ys))
      
let f e =
  toplevel := [];
  let (e', _) = g M.empty S.empty true e in
  Prog(List.rev !toplevel, e')
