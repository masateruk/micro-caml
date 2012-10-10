(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let rec subst ({ Env.tycons = tycons } as env) tyvars t =
  let _ = D.printf "  Typing.subst %s\n" (Type.string_of t) in
  let rec subst' =
    function
    | Type.Var(x) when M.mem x tyvars -> M.find x tyvars
    | Type.Var(x) -> Type.Var(x)
    | Type.Field(tid, t) -> Type.Field(tid, subst' t)
    | Type.Variant(x, ytts) -> Type.Variant(x, (List.map (fun (y, ts) -> y, List.map subst' ts) ytts))
    | Type.App(Type.TyFun(xs, t), ys) -> subst' (subst env (M.add_list2 xs ys M.empty) t)
    | Type.App(Type.NameTycon(x, _), ys) -> subst' (Type.App(M.find x tycons, ys))
    | Type.App(x, ys) -> Type.App(x, (List.map subst' ys))
    | Type.Poly(xs, t) -> 
        let ys = List.map (fun _ -> Type.newtyvar ()) xs in
        let t' = subst env (M.add_list2 xs (List.map (fun y -> Type.Var(y)) ys) M.empty) t in
        Type.Poly(ys, subst' t')
    | Type.Meta{ contents = Some(t) } -> subst' t
    | Type.Meta{ contents = None } as t -> t in
  subst' t
    
let rec occur x = (* occur check (caml2html: typing_occur) *)
  function 
  | Type.App(Type.TyFun(_, u), ts) -> occur x u || List.exists (occur x) ts
  | Type.App(_, ts) -> List.exists (occur x) ts
  | Type.Poly(_, t) -> occur x t
  | Type.Meta{ contents = Some(t) } -> occur x t
  | Type.Meta(y) -> x == y
  | _ -> false
      
let rec unify ({ Env.tycons = tycons } as env) t1 t2 = (* 型が合うように、メタ型変数への代入をする. 成功したら () を返す. (caml2html: typing_unify) *)
  let _ = D.printf "    Typing.unify %s %s\n" (Type.string_of t1) (Type.string_of t2) in
  let rec unify' t1 t2 =  
    match t1, t2 with
    | Type.App(Type.Unit, xs), Type.App(Type.Unit, ys) 
    | Type.App(Type.Bool, xs), Type.App(Type.Bool, ys) 
    | Type.App(Type.Int, xs), Type.App(Type.Int, ys) 
    | Type.App(Type.Arrow, xs), Type.App(Type.Arrow, ys) -> List.iter2 unify' xs ys
    | Type.App(Type.Record(x, fs), xs), Type.App(Type.Record(y, fs'), ys) when fs = fs' -> List.iter2 unify' xs ys
    | Type.App(Type.TyFun(xs, u), ys), t2 -> unify' (subst env (M.add_list2 xs ys M.empty) u) t2
    | t1, Type.App(Type.TyFun(xs, u), ys) -> unify' t1 (subst env (M.add_list2 xs ys M.empty) u)
    | Type.App(Type.NameTycon(x, _), xs), Type.App(Type.NameTycon(y, _), ys) when x = y -> List.iter2 unify' xs ys
    | Type.App(Type.NameTycon(x, { contents = None }), ys), t2 -> unify' (Type.App(M.find x tycons, ys)) t2
    | t1, Type.App(Type.NameTycon(x, { contents = None }), ys) -> unify' t1 (Type.App(M.find x tycons, ys))
    | Type.Poly([], u1), t2 -> unify' u1 t2
    | t1, Type.Poly([], u2) -> unify' t1 u2
    | Type.Poly(xs, u1), Type.Poly(ys, u2) -> unify' u1 (subst env (M.add_list2 ys (List.map (fun x -> Type.Var(x)) xs) M.empty) u2)
    | Type.Var(x), Type.Var(y) when x = y -> ()
    | Type.Field(_, t1'), t2 -> unify' t1' t2
    | t1, Type.Field(_, t2') -> unify' t1 t2'
    | Type.Variant(x, xtss), Type.Variant(y, ytss) when x = y -> List.iter2 (fun (_, xs) (_, ys) -> List.iter2 unify' xs ys) xtss ytss
    | Type.Meta{ contents = Some(t1') }, t2 -> unify' t1' t2
    | Type.Meta(x), Type.Meta{ contents = Some(t2') } -> unify' t1 t2'
    | Type.Meta(x), Type.Meta(y) when x == y -> ()
    | Type.Meta(x), t2 ->
        if occur x t2 then raise (Unify(t1, t2))
        else x := Some(t2)
    | t1, Type.Meta(y) -> unify' t2 t1
    | _, _ -> 
        raise (Unify(t1, t2)) in
  unify' t1 t2
    
let test_unify =
  assert ((unify Env.empty (Type.App(Type.Int, [])) (Type.App(Type.Int, []))) = ())
    
(*        
let rec expand tenv = 
  function
  | Type.App(Type.TyFun(xs, u), ys) -> expand tenv (subst ((M.add_list2 xs ys M.empty), tenv) u)
  | Type.Meta{ contents = Some(t) } -> expand tenv t
  | Type.NameTy(x, _) -> expand tenv (M.find x tenv)
  | t -> t
*)      
let rec generalize { Env.venv = venv; tycons = tycons } t = 
  let _ = D.printf "Typing.generalize %s\n" (Type.string_of t) in
  let rec exists v = 
    function
    | Type.App(Type.NameTycon(_, { contents = Some(tycon) }), ts) -> exists v (Type.App(tycon, ts))
    | Type.App(Type.NameTycon(x, { contents = None }), ts) -> exists v (Type.App(M.find x tycons, ts))
    | Type.App(Type.TyFun(_, u), ts) -> exists v u || List.exists (exists v) ts
    | Type.App(_, ts) -> List.exists (exists v) ts
    | Type.Poly(_, t) -> exists v t
    | Type.Meta{ contents = Some(t') } -> exists v t'
    | Type.Meta(x) when v == x -> true
    | _ -> false in
  let rec metavars vs = 
    function
    | Type.App(Type.TyFun(_, u), ts) -> List.fold_left metavars (metavars vs u) ts
    | Type.App(_, ts) -> List.fold_left metavars vs ts
    | Type.Poly(_, t) -> metavars vs t
    | Type.Meta{ contents = Some(t') } -> metavars vs t'
    | Type.Meta(x) when M.exists (fun _ t' -> exists x t') venv -> vs
    | Type.Meta(x) -> if (List.memq x vs) then vs else x :: vs
    | _ -> vs in
  let ms = metavars [] t in
  let tyvars = List.map 
    (fun m -> 
      match !m with 
      | None -> let var = Type.newtyvar () in 
                m := Some(Type.Var(var)); 
                var 
      | _ -> assert false) 
    ms in
  let t = Type.Poly(tyvars, t) in
  let _ = D.printf "  => %s\n" (Type.string_of t) in
  t
    
let rec instantiate env t =
  let _ = D.printf "Typing.instantiate %s\n" (Type.string_of t) in
  let t = 
    match t with
    | Type.Poly(xs, t) -> 
        subst env (M.add_list (List.map (fun x -> (x, Type.Meta(Type.newmetavar ()))) xs) M.empty) t
    | t -> t in
  let _ = D.printf "  => %s\n" (Type.string_of t) in
  t
      
(* for pretty printing (and type normalization) *)
let rec deref_tycon ({ Env.tycons = tycons } as env) =
  function
  | Type.Int | Type.Bool | Type.Unit | Type.Arrow | Type.Tuple | Type.Record _ as tycon -> tycon
  | Type.NameTycon(x, { contents = Some(tycon) }) -> tycon
  | Type.NameTycon(x, { contents = None }) -> M.find x tycons
  | Type.TyFun(xs, t) -> Type.TyFun(xs, deref_type env t)
  
and deref_type env = (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  function
  | Type.Field(x, t) -> Type.Field(x, deref_type env t)
  | Type.Variant(x, ytss) -> Type.Variant(x, List.map (fun (y, ts) -> y, List.map (deref_type env) ts) ytss)
  | Type.App(x, ys) -> Type.App(deref_tycon env x, List.map (deref_type env) ys)
  | Type.Poly(xs, t) -> Type.Poly(xs, deref_type env t)
  | Type.Meta({ contents = None } as r) -> (* バリアント型のコンストラクタによっては型変数が残る事がある（たとえば、maybeのNothingのときはJust側の型変数が残る）ので *)
      let t = Type.Var(Type.newtyvar ()) in (* そのときはVarにしておく。何でも良いはずなんだけど。*)
      r := Some(t);
      t
  | Type.Meta({ contents = Some(t) } as r) ->
      let t' = deref_type env t in
      r := Some(t');
      t'
  | t -> t

let rec deref_pattern env =
  function
  | PtBool _ | PtInt _ as p -> p
  | PtVar(x, t) -> PtVar(x, deref_type env t)
  | PtTuple(ps) -> PtTuple(List.map (deref_pattern env) ps)
  | PtField(xps) -> PtField(List.map (fun (x, p) -> (x, deref_pattern env p)) xps)
  | PtConstr(x, ps) -> PtConstr(x, List.map (deref_pattern env) ps)
      
let rec deref_id_type env (x, t) = (x, deref_type env t)
let rec deref_term env =
  function
  | Int _ | Bool _ | Unit | Var _ as e -> e
  | Nil(t) -> Nil(deref_type env t)
  | Record(xes) -> Record(List.map (fun (x, e) -> x, deref_term env e) xes)
  | Field(e, x) -> Field(deref_term env e, x)
  | Tuple(es) -> Tuple(List.map (deref_term env) es)
  | Not(e) -> Not(deref_term env e)
  | And(e1, e2) -> And(deref_term env e1, deref_term env e2)
  | Or(e1, e2) -> Or(deref_term env e1, deref_term env e2)
  | Neg(e) -> Neg(deref_term env e)
  | Add(e1, e2) -> Add(deref_term env e1, deref_term env e2)
  | Sub(e1, e2) -> Sub(deref_term env e1, deref_term env e2)
  | Eq(e1, e2) -> Eq(deref_term env e1, deref_term env e2)
  | LE(e1, e2) -> LE(deref_term env e1, deref_term env e2)
  | Mul(e1, e2) -> Mul(deref_term env e1, deref_term env e2)
  | Div(e1, e2) -> Div(deref_term env e1, deref_term env e2)
  | Cons(e1, e2) -> Cons(deref_term env e1, deref_term env e2)
  | If(e1, e2, e3) -> If(deref_term env e1, deref_term env e2, deref_term env e3)
  | Match(e, pes) ->  Match(deref_term env e, (List.map (fun (p, e) -> (deref_pattern env p, deref_term env e)) pes))
  | LetVar(xt, e1, e2) -> LetVar(deref_id_type env xt, deref_term env e1, deref_term env e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_type env xt;
               args = List.map (deref_id_type env) yts;
               body = deref_term env e1 },
             deref_term env e2)
  | App(e, es) -> App(deref_term env e, List.map (deref_term env) es)
  | Constr(x, es) -> Constr(x, List.map (deref_term env) es)
  | WrapBody(x, t) -> WrapBody(x, deref_type env t)
  | UnwrapBody(x, t) -> UnwrapBody(x, deref_type env t)

let rec pattern env (p, t) =
  let _ = D.printf "Typing.pattern (%s, %s)\n" (string_of_pattern p) (Type.string_of t) in
  match p with
  | PtBool(b) -> unify env t (Type.App(Type.Bool, [])); env
  | PtInt(n) -> unify env t (Type.App(Type.Int, [])); env
  | PtVar(x, t') -> 
      unify env t t'; Env.add_var_type env x t
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
  | PtConstr(x, []) -> Env.add_var_type env x t
  | PtConstr(x, ps) -> 
      begin
        match t with
        | Type.Variant(_, ytss) -> 
            let _, ts = List.find (fun (y, _) -> x = y) ytss in
            List.fold_left (fun env' (p, t) -> pattern env' (p, t)) env (List.combine ps ts)
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
      
let rec g ({ Env.venv = venv; types = types; tycons = tycons } as env) e = (* 型推論ルーチン (caml2html: typing_g) *)
  let _ = D.printf "Typing.g %s\n" (string_of_exp e) in
  try
    match e with
    | Unit -> Type.App(Type.Unit, [])
    | Nil _ -> assert false (* not implemented *)
    | Bool(_) -> Type.App(Type.Bool, [])
    | Int(_) -> Type.App(Type.Int, [])
    | Record(xes) -> 
        let xts = List.map (fun (x, e) -> x, g env e) xes in 
        begin
          match M.find (fst (List.hd xts)) types with
          | Type.Poly(ys, Type.Field(rec_typeid, _)) -> 
              begin
                match M.find rec_typeid tycons with
                | Type.TyFun(ys, Type.App(Type.Record(_, _), _)) as rec_type ->
                    let ys' = List.map (fun _ -> Type.Meta(Type.newmetavar ())) ys in                  
                    let rec_type' = subst env M.empty (Type.App(rec_type, ys')) in
                    begin
                      match rec_type' with
                      | Type.App(Type.Record(_), ts') ->
                          List.iter2 (fun (_, t) t' -> unify env t t') xts ts';
                          rec_type'
                      | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
                      end
                | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
              end
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | Field(e, x) ->
        let t = M.find x types in
        begin
          match instantiate env t with
          | Type.Field(rec_typeid, field_type) -> 
              let recty = g env e in
              begin
                match recty with
                | Type.App(Type.Record(_, fields), ts) 
                | Type.Meta({ contents = Some(Type.App(Type.Record(_, fields), ts)) }) ->
                    List.iter2 (fun field t -> if x = field then unify env t field_type else ()) fields ts;
                    instantiate env t
                | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
              end
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | Tuple(es) ->
        Type.App(Type.Tuple, List. map (g env) es)
    | Not(e) ->
        unify env (Type.App(Type.Bool, [])) (g env e);
        Type.App(Type.Bool, [])
    | And(e1, e2) | Or(e1, e2) ->
        unify env (Type.App(Type.Bool, [])) (g env e1);
        unify env (Type.App(Type.Bool, [])) (g env e2);
        Type.App(Type.Bool, [])
    | Neg(e) ->
        unify env (Type.App(Type.Int, [])) (g env e);
        Type.App(Type.Int, [])
    | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
        unify env (Type.App(Type.Int, [])) (g env e1);
        unify env (Type.App(Type.Int, [])) (g env e2);
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
        unify env (g env e1) (g env e2);
        Type.App(Type.Bool, [])
    | If(e1, e2, e3) ->
        unify env (g env e1) (Type.App(Type.Bool, []));
        let t2 = g env e2 in
        let t3 = g env e3 in
        unify env t2 t3;
        t2
    | Match(e, pes) ->
        let t = g env e in
        let ts = List.map (fun (p, e) -> g (pattern env (p, t)) e) pes in
        let t' = List.hd ts in
        List.iter (unify env t') (List.tl ts);
        t'
    | LetVar((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
        let t1 = g env e1 in
        let t1' = generalize env t1 in (* 副作用は未サポートなので、Tiger本のp.335にある代入の判定はなし *)
        unify env t t1';
        g (Env.add_var_type env x t1') e2
    | Var(x) when M.mem x venv -> instantiate env (M.find x venv) (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !Env.extenv -> instantiate env (M.find x !Env.extenv)
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.Meta(Type.newmetavar ()) in
        Env.extenv := M.add x t !Env.extenv;
        instantiate env t
    | Constr(x, []) -> 
        begin
          match M.find x tycons with
          | Type.TyFun(xs, t) -> 
              let xs' = List.map (fun _ -> Type.Meta(Type.newmetavar ())) xs in
              let t' = subst env (M.add_list2 xs xs' M.empty) t in
              t'
          | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
        end
    | Constr(x, es) -> 
        let ts = List.map (g env) es in
        begin
          match M.find x tycons with
          | Type.TyFun(xs, (Type.App(Type.Arrow, ys) as t)) -> 
              let xs' = List.map (fun _ -> Type.Meta(Type.newmetavar ())) xs in
              let t' = subst env (M.add_list2 xs xs' M.empty) t in
              begin
                match t' with
                | Type.App(Type.Arrow, ys) -> 
                    List.iter2 (unify env) ts (L.init ys);
                    (L.last ys)
                | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
              end
          | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
        end
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
        let t2 = Type.Meta(Type.newmetavar()) in
        let t' = Type.App(Type.Arrow, ((List.map snd yts) @ [t2])) in
        let t1 = g { env with Env.venv = M.add_list yts (M.add x t' venv) } e1 in
        unify env t t';
        unify env t2 t1;
        let t'' = generalize env t' in
        g (Env.add_var_type env x t'') e2
    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
        let e' = g env e in
        let es' = List.map (fun t -> g env t) es in
        let result = Type.Meta(Type.newmetavar ()) in
        unify env e' (Type.App(Type.Arrow, es' @ [result]));
        result
    | WrapBody(_, t) -> t
    | UnwrapBody(_, t) -> t
  with Unify(t1, t2) -> 
    let _ = Printf.eprintf "Typing.g error %s : %s and %s\n" (string_of_exp e) (Type.string_of t1) (Type.string_of t2) in
    raise (Error(deref_term env e, deref_type env t1, deref_type env t2))


let f' env (e, t) = 
  (try unify env t (g env e)
   with Unify _ -> failwith "type error.");
  Env.extenv := M.map (deref_type env) !Env.extenv;
  deref_term env e
    
let f = 
  fold (fun ({ Env.venv = venv; types = types; tycons = tycons } as env, defs) def ->
    match def with
    | TypeDef(x, t) -> 
        TypeDef(x, deref_tycon { env with Env.tycons = M.add x t tycons } t) :: defs
    | VarDef((x, t), e) -> 
        let e' = f' env (e, t) in
        VarDef((x, deref_type env t), e') :: defs
    | RecDef({ name = (x, t); args = yts; body = e }) -> 
        let e' = f' { env with Env.venv = M.add_list yts (M.add x t env.Env.venv) } (e, t) in
        RecDef({ name = (x, deref_type env t); args = List.map (fun (y, t) -> (y, deref_type env t)) yts; body = e' }) :: defs)
