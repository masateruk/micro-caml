(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty
  
let rec subst env = 
  let venv, tenv = env in
  function
  | Type.Var(x) when M.mem x venv -> M.find x venv
  | Type.Var(x) -> Type.Var(x)
  | Type.Field(tid, t) -> Type.Field(subst env tid, subst env t)
  | Type.App(Type.TyFun(xs, t), ys) -> subst env (subst ((M.add_list2 xs ys venv), tenv) t)
  | Type.App(x, ys) -> Type.App(x, (List.map (subst env) ys))
  | Type.Poly(xs, t) -> 
      let ys = List.map (fun _ -> Type.newtyvar ()) xs in
      let t' = subst ((M.add_list2 xs (List.map (fun y -> Type.Var(y)) ys) venv), tenv) t in
      Type.Poly(ys, subst env t')
  | Type.Meta{ contents = Some(t) } -> subst env t
  | Type.Meta{ contents = None } as t -> t
  | Type.NameTy(x, { contents = None }) -> subst env (M.find x tenv)
  | Type.NameTy(x, { contents = Some(t) }) -> subst env t
          
let rec occur tenv x = (* occur check (caml2html: typing_occur) *)
  function 
  | Type.App(Type.TyFun(_, u), ts) -> occur tenv x u || List.exists (occur tenv x) ts
  | Type.App(_, ts) -> List.exists (occur tenv x) ts
  | Type.Poly(_, t) -> occur tenv x t
  | Type.Meta{ contents = Some(t) } -> occur tenv x t
  | Type.Meta(y) -> x == y
  | Type.NameTy(y, { contents = None }) -> occur tenv x (M.find y tenv)
  | Type.NameTy(y, { contents = Some(t) }) -> occur tenv x t
  | _ -> false
      
let rec unify tenv t1 t2 = (* 型が合うように、メタ型変数への代入をする. 成功したら () を返す. (caml2html: typing_unify) *)
  let _ = D.printf "  Typing.unify %s %s\n" (Type.string_of t1) (Type.string_of t2) in
  match t1, t2 with
  | Type.App(Type.Unit, xs), Type.App(Type.Unit, ys) 
  | Type.App(Type.Bool, xs), Type.App(Type.Bool, ys) 
  | Type.App(Type.Int, xs), Type.App(Type.Int, ys) 
  | Type.App(Type.Arrow, xs), Type.App(Type.Arrow, ys) -> List.iter2 (unify tenv) xs ys
  | Type.App(Type.Record(x, fs), xs), Type.App(Type.Record(y, fs'), ys) when fs = fs' -> List.iter2 (unify tenv) xs ys
  | Type.App(Type.Variant(_, xtts), xs), Type.App(Type.Variant(_, ytts), ys) -> 
      List.iter2 (fun (_, xs) (_, ys) -> List.iter2 (unify tenv) xs ys) xtts ytts;
      List.iter2 (unify tenv) xs ys
  | Type.App(Type.TyFun(xs, u), ys), t2 -> unify tenv (subst ((M.add_list2 xs ys M.empty), M.empty) u) t2
  | t1, Type.App(Type.TyFun(xs, u), ys) -> unify tenv t1 (subst ((M.add_list2 xs ys M.empty), M.empty) u)
  | Type.Poly([], u1), t2 -> unify tenv u1 t2
  | t1, Type.Poly([], u2) -> unify tenv t1 u2
  | Type.Poly(xs, u1), Type.Poly(ys, u2) -> unify tenv u1 (subst ((M.add_list2 ys (List.map (fun x -> Type.Var(x)) xs) M.empty), M.empty) u2)
  | Type.Var(x), Type.Var(y) when x = y -> ()
  | Type.Field(_, t1'), t2 -> unify tenv t1' t2
  | t1, Type.Field(_, t2') -> unify tenv t1 t2'
  | Type.Meta{ contents = Some(t1') }, t2 -> unify tenv t1' t2
  | Type.Meta(x), Type.Meta{ contents = Some(t2') } -> unify tenv t1 t2'
  | Type.Meta(x), Type.Meta(y) when x == y -> ()
  | Type.Meta(x), t2 ->
      if occur tenv x t2 then raise (Unify(t1, t2))
      else x := Some(t2)
  | t1, Type.Meta(y) -> unify tenv t2 t1
  | Type.NameTy(u1, t), t2 -> 
      let u1' = M.find u1 tenv in 
      t := Some(u1'); 
      unify tenv u1' t2
  | t1, Type.NameTy(u2, t) -> 
      let u2' = M.find u2 tenv in 
      t := Some(u2'); 
      unify tenv t1 u2'
  | _, _ -> 
      raise (Unify(t1, t2))
        
let test_unify =
  assert ((unify M.empty (Type.App(Type.Int, [])) (Type.App(Type.Int, []))) = ())
    
let rec equal tenv t1 t2 = 
  match t1, t2 with
  | Type.App(Type.Unit, xs), Type.App(Type.Unit, ys) 
  | Type.App(Type.Bool, xs), Type.App(Type.Bool, ys) 
  | Type.App(Type.Int, xs), Type.App(Type.Int, ys) 
  | Type.App(Type.Arrow, xs), Type.App(Type.Arrow, ys) -> List.for_all2 (equal tenv) xs ys
  | Type.App(Type.Record(x, _), xs), Type.App(Type.Record(y, _), ys) 
  | Type.App(Type.Variant(x, _), xs), Type.App(Type.Variant(y, _), ys) when List.length xs = List.length ys -> 
      x = y && List.for_all2 (equal tenv) xs ys
  | Type.App(Type.TyFun(xs, u), ys), t2 -> equal tenv (subst ((M.add_list2 xs ys M.empty), M.empty) u) t2
  | Type.Poly([], u1), t2 -> equal tenv u1 t2
  | t1, Type.Poly([], u2) -> equal tenv t1 u2
  | Type.Poly(
    xs, u1), Type.Poly(ys, u2) -> equal tenv u1 (subst ((M.add_list2 ys (List.map (fun x -> Type.Var(x)) xs) M.empty), M.empty) u2)
  | Type.Var(x), Type.Var(y) when x = y -> true
  | Type.Field(_, x), Type.Field(_, y) -> equal tenv x y
  | Type.Meta{ contents = Some(t1') }, t2 -> equal tenv t1' t2
  | Type.Meta(x), Type.Meta{ contents = Some(t2') } -> equal tenv t1 t2'
  | Type.Meta(x), Type.Meta(y) when x == y -> true
  | Type.Meta(x), t2 ->
      if occur tenv x t2 then false
      else true
  | t1, Type.Meta(y) -> equal tenv t2 t1
  | Type.NameTy(u1, t), t2 -> 
      let u1' = M.find u1 tenv in 
      equal tenv u1' t2
  | t1, Type.NameTy(u2, t) -> 
      let u2' = M.find u2 tenv in 
      equal tenv t1 u2'
  | _, _ -> 
      false
        
let rec expand tenv = 
  function
  | Type.App(Type.TyFun(xs, u), ys) -> expand tenv (subst ((M.add_list2 xs ys M.empty), tenv) u)
  | Type.Meta{ contents = Some(t) } -> expand tenv t
  | Type.NameTy(x, _) -> expand tenv (M.find x tenv)
  | t -> t
      
let rec generalize env t = 
  let _ = D.printf "Typing.generalize %s\n" (Type.string_of t) in
  let venv, tenv = env in    
  let rec exists v = 
    function
    | Type.App(Type.TyFun(_, u), ts) -> exists v u || List.exists (exists v) ts
    | Type.App(_, ts) -> List.exists (exists v) ts
    | Type.Poly(_, t) -> exists v t
    | Type.Meta{ contents = Some(t') } -> exists v t'
    | Type.Meta(x) when v == x -> true
    | Type.NameTy(x, _) -> exists v (M.find x tenv)
    | _ -> false in
  let rec metavars vs = 
    function
    | Type.App(Type.TyFun(_, u), ts) -> List.fold_left metavars (metavars vs u) ts
    | Type.App(_, ts) -> List.fold_left metavars vs ts
    | Type.Poly(_, t) -> metavars vs t
    | Type.Meta{ contents = Some(t') } -> metavars vs t'
    | Type.Meta(x) when M.exists (fun _ t' -> exists x t') venv -> vs
    | Type.Meta(x) -> if (List.memq x vs) then vs else x :: vs
    | Type.NameTy(x, _) -> metavars vs (M.find x tenv)
    | _ -> vs in
  let ms = metavars [] t in
  let tyvars = List.map (fun m -> match !m with None -> let var = Type.newtyvar () in m := Some(Type.Var(var)); var | _ -> assert false) ms in
  Type.Poly(tyvars, t)
      
let rec instantiate tenv = 
  function
  | Type.Poly(xs, t) -> 
      subst ((M.add_list (List.map (fun x -> (x, Type.Meta(Type.newmetavar ()))) xs) M.empty), tenv) t
  | Type.NameTy(x, _) -> instantiate tenv (M.find x tenv)
  | t -> t 
      
(* for pretty printing (and type normalization) *)
let rec deref_typ tenv = (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  function
  | Type.App(Type.TyFun(xs, t), ys) -> Type.App(Type.TyFun(xs, deref_typ tenv t), List.map (deref_typ tenv) ys)
  | Type.App(Type.Variant(x, ytss), ts) -> Type.App(Type.Variant(x, List.map (fun (y, ts) -> y, List.map (deref_typ tenv) ts) ytss), List.map (deref_typ tenv) ts)
  | Type.App(x, ys) -> Type.App(x, List.map (deref_typ tenv) ys)
  | Type.Poly(xs, t) -> Type.Poly(xs, deref_typ tenv t)
  | Type.Meta({ contents = None } as r) ->
      Printf.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.App(Type.Int, []));
      Type.App(Type.Int, [])
  | Type.Meta({ contents = Some(t) } as r) ->
      let t' = deref_typ tenv t in
      r := Some(t');
      t'
  | Type.NameTy("bool", _) -> Type.App(Type.Bool, [])
  | Type.NameTy("int", _) -> Type.App(Type.Int, [])
  | Type.NameTy(x, ({ contents = None } as r)) -> 
      let t' = M.find x tenv in
      r := Some(t');
      Type.NameTy(x, r)
  | Type.Field _ -> assert false
  | t -> t
      
let rec deref_id_typ tenv (x, t) = (x, deref_typ tenv t)
let rec deref_term tenv = 
  function
  | Not(e) -> Not(deref_term tenv e)
  | And(e1, e2) -> And(deref_term tenv e1, deref_term tenv e2)
  | Or(e1, e2) -> Or(deref_term tenv e1, deref_term tenv e2)
  | Neg(e) -> Neg(deref_term tenv e)
  | Add(e1, e2) -> Add(deref_term tenv e1, deref_term tenv e2)
  | Sub(e1, e2) -> Sub(deref_term tenv e1, deref_term tenv e2)
  | Eq(e1, e2) -> Eq(deref_term tenv e1, deref_term tenv e2)
  | LE(e1, e2) -> LE(deref_term tenv e1, deref_term tenv e2)
  | Mul(e1, e2) -> Mul(deref_term tenv e1, deref_term tenv e2)
  | Div(e1, e2) -> Div(deref_term tenv e1, deref_term tenv e2)
  | Cons(e1, e2) -> Cons(deref_term tenv e1, deref_term tenv e2)
  | If(e1, e2, e3) -> If(deref_term tenv e1, deref_term tenv e2, deref_term tenv e3)
  | LetVar(xt, e1, e2) -> LetVar(deref_id_typ tenv xt, deref_term tenv e1, deref_term tenv e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_typ tenv xt;
               args = List.map (deref_id_typ tenv) yts;
               body = deref_term tenv e1 },
             deref_term tenv e2)
  | App(e, es) -> App(deref_term tenv e, List.map (deref_term tenv) es)
  | e -> e

let rec pattern ((venv, tenv) as env) (p, t) =
  let _ = D.printf "Typing.pattern (%s, %s)\n" (string_of_pattern p) (Type.string_of t) in
  match p with
  | PtBool(b) -> unify tenv t (Type.App(Type.Bool, [])); env
  | PtInt(n) -> unify tenv t (Type.App(Type.Int, [])); env
  | PtVar(x) -> (M.add x t venv, tenv)
  | PtTuple(ps) -> 
      begin
        match t with
        | Type.App(Type.Tuple, ts) -> List.fold_left (fun env' (p, t) -> pattern env' (p, t)) env (List.combine ps ts)
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
  | PtField(xps) -> 
      begin
        match t with
        | Type.App(Type.Record _, ts) -> List.fold_left (fun env' ((_, p), t) -> pattern env' (p, t)) env (List.combine xps ts)
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
  | PtConstr(x, []) -> (M.add x t venv, tenv)
  | PtConstr(x, ps) -> 
      begin
        match t with
        | Type.App(Type.Variant(_, ytss), []) -> 
            let _, ts = List.find (fun (y, _) -> x = y) ytss in
            List.fold_left (fun env' (p, t) -> pattern env' (p, t)) env (List.combine ps ts)
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
      
let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  let venv, tenv = env in
  let _ = D.printf "Typing.g %s\n" (string_of_exp e) in
  try
    match e with
    | Unit -> Type.App(Type.Unit, [])
    | Nil _ -> assert false (* not implemented *)
    | Bool(_) -> Type.App(Type.Bool, [])
    | Int(_) -> Type.App(Type.Int, [])
    | Record(xes) -> 
        let xts = List.map (fun (x, e) -> x, g env e) xes in 
        List.iter (fun (x, t) -> unify tenv (M.find x tenv) t) xts;
        begin
          match (M.find (fst (List.hd xts)) tenv) with 
          | Type.Field(t, _) -> t 
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | Field(e, x) ->
        let t = M.find x tenv in
        begin
          match t with
          | Type.Field(tid, ys) -> 
              let tid' = g env e in
              unify tenv tid tid';
              t
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | Tuple(es) ->
        Type.App(Type.Tuple, List. map (g env) es)
    | Not(e) ->
        unify tenv (Type.App(Type.Bool, [])) (g env e);
        Type.App(Type.Bool, [])
    | And(e1, e2) | Or(e1, e2) ->
        unify tenv (Type.App(Type.Bool, [])) (g env e1);
        unify tenv (Type.App(Type.Bool, [])) (g env e2);
        Type.App(Type.Bool, [])
    | Neg(e) ->
        unify tenv (Type.App(Type.Int, [])) (g env e);
        Type.App(Type.Int, [])
    | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
        unify tenv (Type.App(Type.Int, [])) (g env e1);
        unify tenv (Type.App(Type.Int, [])) (g env e2);
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
        unify tenv (g env e1) (g env e2);
        Type.App(Type.Bool, [])
    | If(e1, e2, e3) ->
        unify tenv (g env e1) (Type.App(Type.Bool, []));
        let t2 = g env e2 in
        let t3 = g env e3 in
        unify tenv t2 t3;
        t2
    | MATCH(e, pes) ->
        let t = g env e in
        let ts = List.map (fun (p, e) -> g (pattern env (p, t)) e) pes in
        let t' = List.hd ts in
        List.iter (unify tenv t') (List.tl ts);
        t'
    | LetVar((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
        let t1 = g env e1 in
        let t1' = generalize env t1 in (* 副作用は未サポートなので、Tiger本のp.335にある代入の判定はなし *)
        unify tenv t t1';
        g ((M.add x t1' venv), tenv) e2
    | Var(x) when M.mem x venv -> instantiate tenv (M.find x venv) (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> instantiate tenv (M.find x !extenv)
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.Meta(Type.newmetavar ()) in
        extenv := M.add x t !extenv;
        instantiate tenv t
    | Constr(x, []) -> M.find x venv
    | Constr(x, es) -> 
        let ts = List.map (g env) es in
        let () = D.printf "Constr() : %s" (Type.string_of (M.find x venv)) in
        begin
          match (M.find x venv) with
          | Type.App(Type.Arrow, ts') as t -> List.iter2 (unify tenv) ts (L.init ts'); Type.apply t ts
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
        let t2 = Type.Meta(Type.newmetavar()) in
        let t' = Type.App(Type.Arrow, ((List.map snd yts) @ [t2])) in
        let t1 = g ((M.add_list yts (M.add x t' venv)), tenv) e1 in
        unify tenv t t';
        unify tenv t2 t1;
        let t'' = generalize env t' in
        g ((M.add x t'' venv), tenv) e2
    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
        let e' = g env e in
        let es' = List.map (g env) es in
        let result = Type.Meta(Type.newmetavar ()) in
        unify tenv e' (Type.App(Type.Arrow, es' @ [result]));
        result
    | WrapBody(_, t) -> t
    | UnwrapBody(_, t) -> t
  with Unify(t1, t2) -> 
    let _ = D.printf "Typing.g error %s : %s and %s\n" (string_of_exp e) (Type.string_of t1) (Type.string_of t2) in
    raise (Error(deref_term tenv e, deref_typ tenv t1, deref_typ tenv t2))


let f' env (e, t) = 
  let venv, tenv = env in
  (try unify tenv t (g env e)
   with Unify _ -> failwith "type error.");
  extenv := M.map (deref_typ tenv) !extenv;
  deref_term tenv e
    
let f = 
  fold (fun (env, defs) def ->
    let venv, tenv = env in
    match def with
    | TypeDef(x, t) -> 
        TypeDef(x, deref_typ (M.add x t tenv) t) :: defs
    | VarDef((x, t), e) -> 
        let e' = f' env (e, t) in
        VarDef((x, deref_typ tenv t), e') :: defs
    | RecDef({ name = (x, t); args = yts; body = e }) -> 
        let venv, tenv = env in
        let e' = f' ((M.add_list yts (M.add x t venv)), tenv) (e, t) in
        RecDef({ name = (x, deref_typ tenv t); args = List.map (fun (y, t) -> (y, deref_typ tenv t)) yts; body = e' }) :: defs)
