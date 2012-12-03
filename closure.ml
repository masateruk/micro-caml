type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
    term * Type.t
and term =
  | Unit
  | Nil of Type.t
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | MakeCls of (Id.t * Type.t) * closure * t
and e =
    expr * Type.t
and expr = 
  | Bool of bool
  | Int of int
  | Record of (Id.t * e) list
  | Field of e * Id.t
  | Tuple of e list
  | Not of e
  | And of e * e
  | Or of e * e
  | Neg of e
  | Add of e * e
  | Sub of e * e
  | Mul of e * e
  | Div of e * e
  | Eq of e * e
  | LE of e * e
  | Var of Id.t
  | Constr of Id.t * e list
  | App of e * e list
  | AppDir of Id.l * e list
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
type fundef = { name : Id.l * Type.t;
		args : (Id.t * Type.t) list;
		formal_fv : (Id.t * Type.t) list;
		body : t }
and def =
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | FunDef of fundef
type prog = Prog of def list

let rec string_of_pattern =
  function
  | PtBool(b) -> "PtBool(" ^ (string_of_bool b) ^ ")"
  | PtInt(n) -> "PtInt(" ^ (string_of_int n) ^ ")"
  | PtVar(x, t) -> "PtVar(" ^ x ^ "," ^ (Type.string_of t) ^ ")"
  | PtTuple(ps) -> "PtTuple([" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"
  | PtRecord(xps) -> "PtRecord([" ^ (String.concat "; " (List.map (fun (x, p) -> x ^ ", " ^ (string_of_pattern p)) xps)) ^ "])"
  | PtConstr(x, ps) -> "PtConstr(" ^ x ^ ", [" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"

let rec string_of_typed_expr (e, t) = (string_of_expr e) ^ " : " ^ (Type.string_of t)

and string_of_expr = 
  function
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | Record(xes) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xes)) ^ "}"
  | Field(e, x) -> (string_of_typed_expr e) ^ "." ^ x
  | Tuple(es) -> "(" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ ")"
  | Not(e) -> "not " ^ (string_of_typed_expr e)
  | And(e1, e2) -> (string_of_typed_expr e1) ^ " && " ^ (string_of_typed_expr e2)
  | Or(e1, e2) -> (string_of_typed_expr e1) ^ " || " ^ (string_of_typed_expr e2)
  | Neg(e) -> "-" ^ (string_of_typed_expr e)
  | Add(e1, e2) -> (string_of_typed_expr e1) ^ " + " ^ (string_of_typed_expr e2)
  | Sub(e1, e2) -> (string_of_typed_expr e1) ^ " - " ^ (string_of_typed_expr e2)
  | Mul(e1, e2) -> (string_of_typed_expr e1) ^ " * " ^ (string_of_typed_expr e2)
  | Div(e1, e2) -> (string_of_typed_expr e1) ^ " / " ^ (string_of_typed_expr e2)
  | Eq(e1, e2) -> (string_of_typed_expr e1) ^ " = " ^ (string_of_typed_expr e2)
  | LE(e1, e2) -> (string_of_typed_expr e1) ^ " <= " ^ (string_of_typed_expr e2) 
  | Var(x) -> "Var(" ^ x ^ ")"
  | Constr(x, es) -> "Constr(" ^ x ^ ", [" ^ (String.concat "; " (List.map string_of_typed_expr es)) ^ "])"
  | App(e, args) -> "App(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat "; " (List.map string_of_typed_expr args)) ^ "])"
  | AppDir(Id.L(x), args) -> x ^ " " ^ (String.concat " " (List.map string_of_typed_expr args))
      
let rec string_of_typed_term (e, t) = (string_of_term e) ^ " : " ^ (Type.string_of t)

and string_of_term = 
  function
  | Unit -> "Unit"
  | Nil(t) -> "Nil(" ^ (Type.string_of t) ^ ")"
  | WrapBody(x, t) -> "WrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ")"
  | UnwrapBody(x, t) -> "UnwrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ")"
  | Exp(e) -> "Exp(" ^ (string_of_typed_expr e) ^ ")"
  | Cons(s1, s2) -> assert false
  | If(e1, e2, e3) -> "If(" ^ (string_of_typed_expr e1) ^ " then " ^ (string_of_typed_term e2) ^ " else " ^ (string_of_typed_term e3) ^ ")"
  | Match(x, pes) -> "Match(" ^ x ^ ", [" ^ (String.concat "; " (List.map (fun (p, e) -> (string_of_pattern p) ^ " -> " ^ (string_of_typed_term e)) pes)) ^ "])"
  | Let((x, t), e1, e2) -> "LetVar(" ^ x ^ " : " ^ (Type.string_of t) ^ " = " ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"
  | MakeCls((x, t), { entry = Id.L(l); actual_fv = ys }, e) -> "MakeCls(" ^ x ^ " : " ^ (Type.string_of t) ^ " = " ^ l ^ " [" ^ (String.concat ", " ys) ^ "] in " ^ (string_of_typed_term e) ^ ")"
      
let string_of_fundef { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) (yts @ zts))) ^ (* " : " ^ (Id.string_of_typ t) ^ *) " = " ^ (string_of_typed_term e) 
    
let rec string_of_def = 
  function
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.string_of_tycon t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.string_of t) ^ "), " ^ (string_of_typed_term e) ^ ")"
  | FunDef(fundef) -> "FunDef(" ^ (string_of_fundef fundef) ^ ")"

let rec vars_of_pattern = 
  function
  | PtBool _ | PtInt _ -> S.empty
  | PtVar(x, _) -> S.singleton x
  | PtTuple(ps) | PtConstr(_, ps) -> List.fold_left (fun s p -> S.union s (vars_of_pattern p)) S.empty ps
  | PtRecord(xps) -> List.fold_left (fun s (_, p) -> S.union s (vars_of_pattern p)) S.empty xps
      
let rec fv_of_expr (e, _) = 
  match e with
  | Bool(_) | Int(_) -> S.empty
  | Record(xes) -> List.fold_left (fun s (_, e) -> S.union s (fv_of_expr e)) S.empty xes
  | Field(e, _) -> fv_of_expr e
  | Tuple(es) -> List.fold_left (fun s e -> S.union s (fv_of_expr e)) S.empty es
  | Not(e) | Neg(e) -> fv_of_expr e
  | And(e1, e2) | Or(e1, e2) 
  | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Eq(e1, e2) | LE(e1, e2) -> S.union (fv_of_expr e1) (fv_of_expr e2)
  | Var(x) -> S.singleton x
  | Constr(_, es) -> List.fold_left (fun s e -> S.union s (fv_of_expr e)) S.empty es
  | App(e, es) -> List.fold_left (fun s e -> S.union s (fv_of_expr e)) S.empty (e :: es)
  | AppDir(_, es) -> List.fold_left (fun s e -> S.union s (fv_of_expr e)) S.empty es
      
let rec fv (e, _) = 
  match e with
  | Unit | Nil _ | WrapBody _ | UnwrapBody _ -> S.empty
  | Exp(e) -> fv_of_expr e
  | Cons(x, y) -> S.of_list [x; y]
  | If(e, e1, e2) -> S.union (fv_of_expr e) (S.union (fv e1) (fv e2))
  | Match(x, pes) -> (List.fold_left (fun s (p, e) -> S.diff (S.union s (fv e)) (vars_of_pattern p)) S.empty pes)
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
      
let toplevel : def list ref = ref []

let rec pattern env =
  function
  | KNormal.PtBool(b) -> env, PtBool(b)
  | KNormal.PtInt(n) -> env, PtInt(n)
  | KNormal.PtVar(x, t) -> M.add x t env, PtVar(x, t)
  | KNormal.PtTuple(ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtTuple(ps')
  | KNormal.PtField(xps) -> 
      let env, xps' = List.fold_left (fun (env, xps) (x, p) -> let env', p' = pattern env p in env', (x, p') :: xps) (env, []) (List.rev xps) in
      env, PtRecord(xps')
  | KNormal.PtConstr(x, ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtConstr(x, ps')

let rec h env known (e, t) = 
  let () = D.printf "Closure.h %s\n" (KNormal.string_of_expr e) in
  let e' =
    match e with
    | KNormal.Bool(b) -> Bool(b)
    | KNormal.Int(i) -> Int(i)
    | KNormal.Record(xes) -> Record(List.map (fun (x, e) -> x, h env known e) xes)
    | KNormal.Field(e, x) -> Field(h env known e, x)
    | KNormal.Tuple(es) -> Tuple(List.map (h env known) es)
    | KNormal.Var(x) -> Var(x)
    | KNormal.Constr(x, es) -> Constr(x, List.map (h env known) es)
    | KNormal.Not(e) -> Not(h env known e)
    | KNormal.And(e1, e2) -> And(h env known e1, h env known e2)
    | KNormal.Or(e1, e2) -> Or(h env known e1, h env known e2)
    | KNormal.Neg(e) -> Neg(h env known e)
    | KNormal.Add(e1, e2) -> Add(h env known e1, h env known e2)
    | KNormal.Sub(e1, e2) -> Sub(h env known e1, h env known e2)
    | KNormal.Mul(e1, e2) -> Mul(h env known e1, h env known e2)
    | KNormal.Div(e1, e2) -> Div(h env known e1, h env known e2)
    | KNormal.Eq(e1, e2) -> Eq(h env known e1, h env known e2)
    | KNormal.LE(e1, e2) -> LE(h env known e1, h env known e2)
    | KNormal.App((KNormal.Var(x), ft), ys) when S.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
        D.printf "directly applying %s@." x;
        AppDir(Id.L(x), List.map (h env known) ys)
    | KNormal.App(e, es) -> 
        App(h env known e, List.map (h env known) es)
    | KNormal.ExtFunApp(x, ys) -> 
        AppDir(Id.L(x), List.map (h env known) ys) in
  (e', t)
  
let rec g env known (e, t) = (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  let () = D.printf "Closure.g %s\n" (KNormal.string_of_term e) in
  let e' = 
    match e with 
    | KNormal.Unit -> Unit
    | KNormal.Nil(t) -> Nil(t)
    | KNormal.WrapBody(x, t) -> WrapBody(x, t)
    | KNormal.UnwrapBody(x, t) -> UnwrapBody(x, t)
    | KNormal.Exp(e) -> Exp(h env known e)
    | KNormal.Cons(x, y) -> assert false (* not implemented *)
    | KNormal.If(e, e1, e2) -> If(h env known e, g env known e1, g env known e2)
    | KNormal.Match(x, pes) -> Match(x, (List.map (fun (p, e) -> let env', p' = pattern env p in p', (g env' known e)) pes))
    | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
    | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	 xに自由変数がない(closureを介さずdirectに呼び出せる)
	 と仮定し、knownに追加してe1をクロージャ変換してみる *)
        let toplevel_backup = !toplevel in
        let env' = M.add x t env in
        let known' = S.add x known in
        let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
        let zs = S.diff (fv e1') 
	  (S.of_list ((List.map fst yts) @ 
		         (List.fold_left 
			    (fun zs def -> 
			      match def with 
			      | TypeDef _ -> zs
			      | VarDef((x, _), _) | FunDef{ name = (Id.L(x), _); _ } -> x :: zs)
			    [] !toplevel))) in
        let known', e1' =
	  if S.is_empty zs then 
          known', e1' else
	  (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
	  (D.printf "free variable(s) %s found in function %s@.\n" (Id.pp_list (S.elements zs)) x;
	   D.printf "function %s cannot be directly applied in fact@.\n" x;
	   toplevel := toplevel_backup;
	   let e1' = g (M.add_list yts env') known e1 in
	   known, e1') in
        let zs = S.elements (S.diff (fv e1') 
			       (S.add x (S.of_list ((List.map fst yts) @ 
						       (List.fold_left 
							  (fun zs def -> 
							    match def with 
							    | TypeDef _ -> zs
							    | VarDef((x, _), _) | FunDef{ name = (Id.L(x), _); _ } -> x :: zs)
							  [] !toplevel))))) in
        let zts = List.map (fun z -> z, M.find z env') zs in (* ここで自由変数zの型を引くために引数envが必要 *)
        toplevel := FunDef{ name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
        let e2' = g env' known' e2 in
        if S.mem x (fv e2') && (zs <> []) then (* xが変数としてe2'に出現するか。自由変数がないときは関数のまま使用する *)
	MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
        else
	(D.printf "eliminating closure(s) %s@.\n" x;
	 fst e2') (* 出現しなければMakeClsを削除 *) in
  (e', t)
      
let f' { Env.venv = venv } e = g venv S.empty e

let f defs =
  toplevel := [];
  let _ = KNormal.fold
    (fun ({ Env.venv = venv; types = types; tycons = tycons } as env, defs) def ->
      let env', def' = 
        match def with 
        | KNormal.TypeDef(x, t) -> 
            { env with
              Env.types  = M.add_list (Type.types t) types;
              Env.tycons = M.add_list ((x, t) :: (Type.tycons t)) tycons },
          TypeDef(x, t)
        | KNormal.VarDef((x, t), e) -> 
            Env.add_var_type env x t, (VarDef((x, t), f' env e))
        | KNormal.RecDef({ KNormal.name = (x, t); args = yts; body = e1 }) ->
            let env' = Env.add_var_type env x t in
            env', (FunDef({ name = (Id.L(x), t); args = yts; formal_fv = []; body = f' env' e1 })) in
      toplevel := def' :: !toplevel;
      (env', def' :: defs))
    Env.empty defs in
  Prog(List.rev !toplevel)
    
