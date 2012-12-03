(* 多相型をラップするモジュール *)

open Syntax

let wrappers = Hashtbl.create 64
let unwrappers = Hashtbl.create 64

let wrapper_defs = ref []
  
let gen_wrapper s t = 
  let x, t' = 
    match t with
    | Type.App(Type.Bool, []) 
    | Type.App(Type.Int, []) 
    | Type.App(Type.Record(_, _), _) 
    | Type.App(Type.Tuple, _) -> 
        "_wrap_" ^ (Type.name t), t
    | Type.App(Type.Arrow, us) -> 
        "_wrap_closure" ^ (Id.genid ""), (Type.App(Type.Arrow, (List.map (fun _ -> (Type.Var(Type.newtyvar ()))) us)))
    | Type.Variant(x, _) ->
        "_wrap_" ^ x ^ (Id.genid ""), t
    | t -> Printf.eprintf "not implemented. t = %s\n" (Type.string_of t); assert false in
  let ft = Type.App(Type.Arrow, [t'; s]) in  
  let y = Id.gentmp (Type.prefix t') in  
  (Var(x), ft), RecDef({ name = (x, ft); args = [(y, t')]; body = WrapBody(y, t'), t })
    
let gen_unwrapper s t = 
  let x, t = 
    match t with
    | Type.App(Type.Bool, []) 
    | Type.App(Type.Int, []) 
    | Type.App(Type.Record(_, _), _) 
    | Type.App(Type.Tuple, _) -> 
        "_unwrap_" ^ (Type.name t), t
    | Type.App(Type.Arrow, vs) ->
        "_unwrap_closure" ^ (Id.genid ""), (Type.App(Type.Arrow, (List.map (fun _ -> Type.Var(Type.newtyvar ())) vs)))
    | Type.Variant(x, _) ->
        "_unwrap_" ^ x ^ (Id.genid ""), t
    | t -> Printf.eprintf "not implemented. t = %s\n" (Type.string_of t); assert false in
  let ft = Type.App(Type.Arrow, [s; t]) in  
  let y = Id.gentmp (Type.prefix s) in  
  (Var(x), ft), RecDef({ name = (x, ft); args = [(y, s)]; body = UnwrapBody(y, t), t })
    
let find_wrapper table s t generator =
  try
    Hashtbl.find table t
  with
    Not_found ->
      let expr, def = generator s t in
      Hashtbl.add table t expr;
      wrapper_defs := def :: !wrapper_defs;
      expr
        
let wrapper s t = find_wrapper wrappers t s gen_wrapper
let unwrapper s t = find_wrapper unwrappers s t gen_unwrapper

let rec has_tyvar t = 
  match t with
  | Type.Var _ -> true
  | Type.Field(_, s) -> has_tyvar s
  | Type.Variant(_, ytss) -> List.exists (fun (y, ts) -> (List.exists has_tyvar ts)) ytss
  | Type.App(Type.TyFun(_, s), us) -> (has_tyvar s) || (List.exists has_tyvar us)
  | Type.App(_, us) -> List.exists has_tyvar us
  | Type.Poly(_, s) -> assert false
  | Type.Meta({ contents = None }) -> false
  | Type.Meta({ contents = Some(t') }) -> has_tyvar t'
      
(* 関数適応時の引数の包み込み。Tiger本の p.345 *)
let rec wrap (e, s) t = 
  let _ = D.printf "Wrap.wrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_expr e) (Type.string_of s) (Type.string_of t) in
  let e', t' =
    match s, t with
    | s, t when Type.equal s t -> e, t
    | Type.Field(_, s), t -> wrap (e, s) t
    | s, Type.Field(_, t) -> wrap (e, s) t
    | Type.Var _, Type.Var _ -> e, t
    | Type.App(Type.Bool, []), Type.Var _ 
    | Type.App(Type.Int, []), Type.Var _ 
    | Type.App(Type.Record _, _), Type.Var _ 
    | Type.App(Type.Tuple, _), Type.Var _ 
    | Type.Variant(_), Type.Var _ ->
        App(wrapper s t, [(e, s)]), t
    | Type.App(Type.Arrow, us), Type.Var _ -> 
        let name = Id.genid "_fw" in
        let yts = List.map (fun u -> (Id.gentmp (Type.prefix u), Type.Var(Type.newtyvar ()))) (L.init us) in
        let r = Type.Var(Type.newtyvar ()) in
        let e' = 
          App(wrapper s t, 
              let ft = Type.App(Type.Arrow, (List.map snd yts) @ [r]) in
              [(LetRec({ name = (name, ft); 
                         args = yts;
                         body = wrap (App((e, s), List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r },
                       (Var(name), ft)), ft)]) in
        e', t
    | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar t ->
        let name = Id.genid "_fw" in
        let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
        begin
          match (L.last vs) with
          | Type.Var _ -> 
              let ft = Type.App(Type.Arrow, (List.map snd yts) @ [L.last vs]) in
              LetRec({ name = (name, ft); 
                       args = yts;
                       body = wrap (App((e, s), List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
                     (Var(name), ft)), ft
          | r -> 
              let ft = Type.App(Type.Arrow, (List.map snd yts) @ [r]) in
              LetRec({ name = (name, ft); 
                       args = yts;
                       body = unwrap (App((e, s), List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r }, 
                     (Var(name), ft)), ft
        end
    | s, t -> Printf.eprintf "not implemented. \ns = %s\nt = %s\n" (Type.string_of s) (Type.string_of t); assert false in
  e', t'
    
and unwrap (e, s) t = 
  let _ = D.printf "Wrap.unwrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_expr e) (Type.string_of s) (Type.string_of t) in
  let e', t' =
    match s, t with
    | s, t when Type.equal s t -> e, t
    | Type.Var _, Type.Var _ -> e, t
    | Type.Var _, Type.App(Type.Bool, []) 
    | Type.Var _, Type.App(Type.Int, []) 
    | Type.Var _, Type.App(Type.Record _, _) 
    | Type.Var _, Type.App(Type.Tuple, _) 
    | Type.Var _, Type.Variant(_) ->
        App(unwrapper s t, [(e, s)]), t
    | Type.Var _, Type.App(Type.Arrow, vs) ->
        let e' = 
          let name = Id.genid "_fu" in
          let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
          LetRec({ name = (name, t);
                   args = yts;
                   body = let t' = List.map (fun _ -> Type.Var(Type.newtyvar ())) vs in
                          let e' = App(unwrapper s (Type.App(Type.Arrow, t')), [(e, s)]), (Type.App(Type.Arrow, t')) in
                          unwrap (App(e', (List.map2 (fun (y, t) t' -> wrap (Var(y), t) t') yts (L.init t'))), (L.last t')) (L.last vs) },
                 (Var(name), t)) in
        e', t
    | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar s ->
        let name = Id.genid "_fu" in
        let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
        LetRec({ name = (name, t); 
                 args = yts;
                 body = unwrap (App((e, s), List.map2 (fun (y, t) u -> wrap (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
               (Var(name), t)), t
    | _ -> Printf.eprintf "not implemented.\n"; assert false
  in
  e', t'
 
let subst_map s t =
  let _ = D.printf "Wrap.subst_map %s %s\n" (Type.string_of s) (Type.string_of t) in
  let rec loop s t xs =
    match s, t with 
    | Type.Var(v), _ -> (v, t) :: xs
    | Type.App(Type.TyFun(_, s'), us), Type.App(Type.TyFun(_, t'), vs) -> 
        let xs' = loop s t xs in
        List.fold_left2 (fun xs u v -> loop u v xs) xs' us vs
    | Type.App(_, us), Type.App(_, vs) ->
        List.fold_left2 (fun xs u v -> loop u v xs) xs us vs
    | s, t -> xs in (* Printf.eprintf "invalid type : s = %s\n  t = %s\n" (Type.string_of_typ s) (Type.string_of_typ t); assert false in *)
  loop s t []

let rec pattern ({ Env.venv = venv; types = types; tycons = tycons } as env) p =    
  match p with
  | PtBool(b) -> env, Type.App(Type.Bool, [])
  | PtInt(n) -> env, Type.App(Type.Int, [])
  | PtVar(x, t) -> Env.add_var_type env x t, t
  | PtTuple(ps) -> 
      let env, _, ts' = List.fold_left (fun (env, ps, ts) p -> let env', t' = pattern env p in env', p :: ps, t' :: ts) (env, [], []) (List.rev ps) in
      env, Type.App(Type.Tuple, ts')
  | PtRecord(xps) -> 
      let env, _, ts' = List.fold_left (fun (env, xps, ts) (x, p) -> let env', t' = pattern env p in env', (x, p) :: xps, t' :: ts) (env, [], []) (List.rev xps) in
      begin
        match M.find (fst (List.hd xps)) types with
        | Type.Poly(_, Type.Field(t, _)) ->
            begin 
              match t with
              | Type.App(Type.Record(name, fields), _) ->
                  env, Type.App(Type.Record(name, fields), ts')
              | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
            end
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
      end
  | PtConstr(x, ps) -> 
      let env, _, ts' = List.fold_left (fun (env, ps, ts) p -> let env', t' = pattern env p in env', p :: ps, t' :: ts) (env, [], []) (List.rev ps) in
      begin
        match M.find x tycons with
        | Type.TyFun(_, Type.App(Type.Arrow, _)) ->
            env, Type.App(Type.Arrow, ts')
        | Type.TyFun(_, (Type.Variant _ as t)) ->
            assert (ps = []);
            env, t
        | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
      end

let rec instantiate =
  function
  | Type.Poly(_, t) -> t
  | t -> t

let rec g ({ Env.venv = venv; types = types; tycons = tycons } as env) (e, t) =
  let _ = D.printf "Wrap.g %s\n" (string_of_expr e) in

  let unary e f =
    let e' = g env e in
    f (unwrap e' (snd e)) in

  let binop e1 e2 f =
    let e1' = g env e1 in
    let e2' = g env e2 in
    f (unwrap e1' (snd e1)) (unwrap e2' (snd e2)) in

  let e', t' = 
    match e with
    | Unit | Bool _ | Int _ -> e, t
    | Record(xets) -> 
        let xets', ts' = List.fold_left (fun (xets, ts) (x, e) -> let e', t' = g env e in (x, (e', t')) :: xets, t' :: ts) ([], []) (List.rev xets) in 
        begin
          match M.find (fst (List.hd xets)) types with
          | Type.Poly(xs, Type.Field(rt, _)) -> 
              begin 
                match rt with 
                | Type.App(Type.Record(_, _), ts) ->
                    let xets' = List.map2 (fun (x, et') t -> x, wrap et' t) xets' ts in
                    (unwrap (Record(xets'), rt) rt)
                | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
              end
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
        end
    | Field(e, x) -> 
        let e' = g env e in       
        (match (snd e') with 
        | Type.App(Type.Record(r, xs), ts) -> 
            let t = List.assoc x (List.combine xs ts) in
            let t' = instantiate t in
            (Field(e', x), t')
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false)
    | Tuple(es) -> 
        let es' = List.map (g env) es in
        Tuple(es'), Type.App(Type.Tuple, List.map snd es')
    | Not(e) -> unary e (fun e' -> Not(e'), t)
    | And(e1, e2) -> binop e1 e2 (fun e1' e2' -> And(e1', e2'), t)
    | Or(e1, e2) -> binop e1 e2 (fun e1' e2' -> Or(e1', e2'), t)
    | Neg(e) -> unary e (fun e' -> Neg(e'), t)
    | Add(e1, e2) -> binop e1 e2 (fun e1' e2' -> Add(e1', e2'), t)
    | Sub(e1, e2) -> binop e1 e2 (fun e1' e2' -> Sub(e1', e2'), t)
    | Mul(e1, e2) -> binop e1 e2 (fun e1' e2' -> Mul(e1', e2'), t)
    | Div(e1, e2) -> binop e1 e2 (fun e1' e2' -> Div(e1', e2'), t)
    | Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Eq(e1', e2'), t)
    | LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> LE(e1', e2'), t)
    | If(e1, e2, e3) -> 
        let e1' = g env e1 in
        let e2' = g env e2 in
        let e3' = g env e3 in
        if (Type.equal (snd e2') (snd e3')) then
        If(unwrap e1' (Type.App(Type.Bool, [])), e2', e3'), (snd e2')
        else 
        If(unwrap e1' (Type.App(Type.Bool, [])), unwrap e2' (snd e2), unwrap e3' (snd e3)), t
    | Match(e, pes) -> 
        let e' = g env e in
        let pes' = List.map (fun (p, e) -> p, g (fst (pattern env p)) e) pes in
        let (_, (_, t1)) = List.hd pes' in
        if (List.for_all (fun (_, (_, t)) -> Type.equal t t1) (List.tl pes')) then
        Match(e', pes'), t1
        else
        Match(e', List.map2 (fun (_, (_, t)) (p', e') -> p', unwrap e' t) pes pes'), t
    | LetVar((x, t), e1, e2) -> 
        let e1' = g env e1 in
        let e2' = g (Env.add_var_type env x t) e2 in
        LetVar((x, t), e1', e2'), (snd e2')
    | Var(x) when M.mem x !Env.extenv -> Var(x), instantiate (M.find x !Env.extenv)
    | Var(x) -> Var(x), instantiate (M.find x venv)
    | Constr(x, []) -> Constr(x, []), t
    | Constr(x, es) -> 
        let ets' = List.map (g env) es in
        begin
          match M.find x tycons with
          | Type.TyFun(xs, (Type.App(Type.Arrow, ys) as t)) -> 
              let t' = Typing.subst env (List.fold_left2 (fun env y (_, t) -> M.add_list (subst_map y t) env) M.empty (L.init ys) ets') t in
              let r', ys' = match t' with Type.App(Type.Arrow, ys') -> L.last ys', ys' | _ -> assert false in
              (unwrap (Constr(x, List.map2 wrap ets' (L.init ys)), (L.last ys)) r')
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_tycon t); assert false
        end
    | LetRec({ name = (x, ft); args = yts; body = e1 }, e2) -> 
        let e1' = g { env with Env.venv = M.add_list yts (M.add x ft venv) } e1 in
        let e2' = g (Env.add_var_type env x ft) e2 in
        LetRec({ name = (x, ft); args = yts; body = e1' }, e2'), (snd e2')
    | App(e, es) -> 
        let e' = g env e in
        let ets' = List.map (g env) es in 
        (match (snd e') with 
        | Type.App(Type.Arrow, ys) ->
            let t' = Typing.subst env (List.fold_left2 (fun env y (_, t) -> M.add_list (subst_map y t) env) M.empty (L.init ys) ets') (Type.App(Type.Arrow, ys)) in
            let r', ys' = match t' with Type.App(Type.Arrow, ys') -> L.last ys', ys' | _ -> assert false in
            (unwrap (App(e', List.map2 wrap ets' (L.init ys)), (L.last ys)) r')
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false)
    | WrapBody _ | UnwrapBody _ -> Printf.eprintf "impossible.\n"; assert false 
    | Cons _ | Nil _ -> Printf.eprintf "not implemented.\n"; assert false 
  in
  e', t'
    
let f' env e = g env e 
    
let f defs = 
  let defs' = 
    fold (fun (env, defs) def -> 
      match def with
      | TypeDef(x, t) -> TypeDef(x, t) :: defs
      | VarDef((x, t), e) -> 
          let e' = f' env e in
          let defs' = !wrapper_defs @ defs in
          wrapper_defs := [];
          VarDef((x, t), e') :: defs'
      | RecDef({ name = (x, t); args = yts; body = e }) -> 
          let e' = f' env e in
          let defs' = !wrapper_defs @ defs in
          wrapper_defs := [];
          RecDef({ name = (x, t); args = yts; body = e' }) :: defs')
      defs in
  defs'
