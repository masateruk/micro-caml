(* 多相型をラップするモジュール *)

open Syntax
  
let wrappers = Hashtbl.create 64
let unwrappers = Hashtbl.create 64

let wrapper_defs = ref []
  
let gen_wrapper s t = 
  let x, t' = 
    match s, t with
    | Type.Var _, Type.App(Type.Bool, []) 
    | Type.Var _, Type.App(Type.Int, []) 
    | Type.Var _, Type.App(Type.Record(_, _), _) 
    | Type.Var _, Type.App(Type.Tuple, _) -> "_wrap_" ^ (Type.name t), t
    | Type.Var _, Type.App(Type.Arrow, us) -> 
        "_wrap_closure" ^ (Id.genid ""), (Type.App(Type.Arrow, (List.map (fun _ -> (Type.Var(Type.newtyvar ()))) us)))
    | Type.Var _, Type.Variant(x, _) ->
        "_wrap_" ^ x ^ (Id.genid ""), t
    | _ -> Printf.eprintf "not implemented.\n"; assert false in
  let ft = Type.App(Type.Arrow, [t'; Type.Var(Type.newtyvar ())]) in  
  let y = Id.gentmp (Type.prefix t') in  
  x, RecDef({ name = (x, ft); args = [(y, t')]; body = WrapBody(y, t') })
    
let gen_unwrapper s t = 
  let x, s, t = 
    match s, t with
    | Type.Var _, Type.App(Type.Bool, []) 
    | Type.Var _, Type.App(Type.Int, []) 
    | Type.Var _, Type.App(Type.Record(_, _), _) 
    | Type.Var _, Type.App(Type.Tuple, _) -> "_unwrap_" ^ (Type.name t), s, t
    | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) -> 
        "_unwrap_closure" ^ (Id.genid ""), Type.Var(Type.newtyvar ()), (Type.App(Type.Arrow, (List.map (fun _ -> Type.Var(Type.newtyvar ())) vs)))
    | Type.Var _, Type.Variant(x, _) ->
        "_unwrap_" ^ x, s, t
    | Type.App(Type.Record(x, _), _), Type.App(Type.Record(y, _), _) ->
        "_unwrap_" ^ y ^ (Id.genid ""), s, t
    | Type.Variant(x, _), Type.Variant(y, _) ->
        "_unwrap_" ^ y ^ (Id.genid ""), s, t
    | s, t -> Printf.eprintf "not implemented. s = %s, t = %s\n" (Type.string_of s) (Type.string_of t); assert false in
  let ft = Type.App(Type.Arrow, [s; t]) in  
  let y = Id.gentmp (Type.prefix s) in  
  x, RecDef({ name = (x, ft); args = [(y, s)]; body = UnwrapBody(y, t) })
    
let get_wrapper f table s t =
  try
    Hashtbl.find table t
  with
    Not_found ->
      let name, def = f s t in
      Hashtbl.add table t name;
      wrapper_defs := def :: !wrapper_defs;
      name
        
let wrapper s t = get_wrapper gen_wrapper wrappers t s
let unwrapper s t = get_wrapper gen_unwrapper unwrappers s t

let rec has_tyvar t = 
  match t with
  | Type.Var _ -> true
  | Type.Field(_, s) -> has_tyvar s
  | Type.Variant(_, ytss) -> List.exists (fun (y, ts) -> (List.exists has_tyvar ts)) ytss
  | Type.App(Type.TyFun(_, s), us) -> (has_tyvar s) || (List.exists has_tyvar us)
  | Type.App(_, us) -> List.exists has_tyvar us
  | Type.Poly(_, s) -> has_tyvar s
  | Type.Meta({ contents = None }) -> false
  | Type.Meta({ contents = Some(t') }) -> has_tyvar t'
      
(* 関数適応時の引数の包み込み。Tiger本の p.345 *)
let rec wrap (e, s) t = 
  let _ = D.printf "Wrap.wrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_exp e) (Type.string_of s) (Type.string_of t) in
  match s, t with
  | Type.Poly([], s), t -> wrap (e, s) t
  | s, Type.Poly([], t) -> wrap (e, s) t
  | Type.Field(_, s), t -> wrap (e, s) t
  | s, Type.Field(_, t) -> wrap (e, s) t
  | Type.Var _, Type.Var _ -> e
  | Type.App(Type.Bool, []), Type.Var _ 
  | Type.App(Type.Int, []), Type.Var _ 
  | Type.App(Type.Record _, _), Type.Var _ 
  | Type.App(Type.Tuple, _), Type.Var _ 
  | Type.Variant(_), Type.Var _ ->
      App(Var(wrapper s t), [e])
  | Type.App(Type.Arrow, us), Type.Var _ -> 
      let yts = List.map (fun u -> (Id.gentmp (Type.prefix u), Type.Var(Type.newtyvar ()))) (L.init us) in
      let r = Type.Var(Type.newtyvar ()) in
      let e' = 
        App(Var(wrapper s t), 
            [LetRec({ name = ("_fw", Type.App(Type.Arrow, (List.map snd yts) @ [r])); 
                      args = yts;
                      body = wrap (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r },
                    Var("_fw"))]) in
      e'
  | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar t ->
      let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
      begin
        match (L.last vs) with
        | Type.Var _ -> 
            LetRec({ name = ("_fw", Type.App(Type.Arrow, (List.map snd yts) @ [L.last vs])); 
                     args = yts;
                     body = wrap 
                (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
                   Var("_fw"))
        | r -> 
            LetRec({ name = ("_fw", Type.App(Type.Arrow, (List.map snd yts) @ [r])); 
                     args = yts;
                     body = unwrap (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r }, 
                   Var("_fw"))
      end
  | s, t when Type.equal s t -> e
  | s, t -> Printf.eprintf "not implemented. \ns = %s\nt = %s\n" (Type.string_of s) (Type.string_of t); assert false
    
and unwrap (e, s) t = 
  let _ = D.printf "Wrap.unwrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_exp e) (Type.string_of s) (Type.string_of t) in
  match s, t with
  | s, t when s = t -> e
  | Type.Poly([], s), t -> unwrap (e, s) t
  | s, Type.Poly([], t) -> unwrap (e, s) t
  | Type.Var _, Type.Var _ -> e
  | Type.Var _, Type.App(Type.Bool, []) 
  | Type.Var _, Type.App(Type.Int, []) 
  | Type.Var _, Type.App(Type.Record _, _) 
  | Type.Var _, Type.App(Type.Tuple, _) 
  | Type.Var _, Type.Variant(_) ->
      App(Var(unwrapper s t), [e])
  | Type.Var _, Type.App(Type.Arrow, vs) ->
      let e' = 
        let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
        LetRec({ name = ("_fu", t);
                 args = yts;
                 body =
            let t' = List.map (fun _ -> Type.Var(Type.newtyvar ())) vs in
            let e' = unwrap (e, Type.App(Type.Arrow, t')) t in
            unwrap (App(e', (List.map2 (fun (y, t) t' -> wrap (Var(y), t) t') yts (L.init t'))), (L.last t')) (L.last vs) },
               Var("_fu")) in
      e'
  | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar s ->
      App(Var(unwrapper s t), [e])
  | Type.App(Type.Record(x, _), _), Type.App(Type.Record(y, _), _) when x = y && (has_tyvar s) ->
      App(Var(unwrapper s t), [e])
  | Type.Variant(x, xtss), Type.Variant(y, ytss) when x = y && (has_tyvar s) ->
      App(Var(unwrapper s t), [e])
  | _ -> Printf.eprintf "not implemented.\n"; assert false
 
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
    
let rec pattern env (p, t) =
  match p with
  | PtBool(b) -> env
  | PtInt(n) -> env
  | PtVar(x, t) -> Env.add_var_type env x t
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

let rec g ({ Env.venv = venv; types = types; tycons = tycons } as env) e =
  let _ = D.printf "Wrap.g %s\n" (string_of_exp e) in
  match e with
  | Unit -> Unit, Type.App(Type.Unit, [])
  | Bool(b) -> Bool(b), Type.App(Type.Bool, [])
  | Int(n) -> Int(n), Type.App(Type.Int, [])
  | Record(xes) -> 
      let xets' = List.map (fun (x, e) -> let e', t' = g env e in x, (e', t')) xes in 
      begin
        match M.find (fst (List.hd xes)) types with
        | Type.Poly(_, Type.Field(x, _)) -> 
            begin 
              match M.find x tycons with 
              | Type.TyFun(tyvars, Type.App(Type.Record(x, ys), ts)) as tycon -> 
                  let t = Typing.subst env M.empty (Type.App(tycon, List.map (fun v -> Type.Var(v)) tyvars)) in
                  let ts = (match t with Type.App(Type.Record(x, ys), ts) -> ts | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false) in
                  let xes' = List.map2 (fun (x, (e, s)) t -> x, wrap (e, s) t) xets' ts in
                  let t' = Type.App(Type.Record(x, ys), List.map (fun (x, (e, t)) -> t) xets') in
                  (unwrap (Record(xes'), t) t'), t'
              | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
            end
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false
      end
  | Field(e, x) -> 
      let e', t = g env e in       
      (match t with 
      | Type.App(Type.Record(r, xs), ts) -> 
          begin
            match M.find r tycons with
            | Type.TyFun(tyvars, Type.App(Type.Record(_, _), ts')) ->
                let t = List.assoc x (List.combine xs ts) in
                (Field(e', x), t)
            | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
          end
      | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false)
  | Tuple(es) -> 
      let ets' = List.map (g env) es in
      Tuple(List.map fst ets'), Type.App(Type.Tuple, List.map snd ets')
  | Not(e) -> let e', _ = g env e in Not(e'), Type.App(Type.Bool, [])
  | And(e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      And(e1', e2'), Type.App(Type.Bool, [])
  | Or(e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      Or(e1', e2'), Type.App(Type.Bool, [])
  | Neg(e) -> let e', _ = g env e in Neg(e'), Type.App(Type.Int, [])
  | Add(e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      Add(e1', e2'), Type.App(Type.Int, [])
  | Sub(e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      Sub(e1', e2'), Type.App(Type.Int, [])
  | Mul(e1, e2) ->
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      Mul(e1', e2'), Type.App(Type.Int, [])
  | Div(e1, e2) ->
      let e1', _ = g env e1 in
      let e2', _ = g env e2 in
      Div(e1', e2'), Type.App(Type.Int, [])
  | Eq(e1, e2) -> 
      let e1', t1 = g env e1 in
      let e2', _ = g env e2 in
      Eq(e1', e2'), t1
  | LE(e1, e2) -> 
      let e1', t1 = g env e1 in
      let e2', _ = g env e2 in
      LE(e1', e2'), t1
  | If(e1, e2, e3) -> 
      let e1', _ = g env e1 in
      let e2', t2 = g env e2 in
      let e3', _ = g env e3 in
      If(e1', e2', e3'), t2
  | Match(e, pes) -> 
      let e', t' = g env e in
      let pets = List.map 
        (fun (p, e) -> 
          let e', t = g (pattern env (p, t')) e in
          (p, e'), t) pes in
      Match(e', List.map fst pets), snd (List.hd pets)
  | LetVar((x, t), e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', t2 = g (Env.add_var_type env x t) e2 in
      LetVar((x, t), e1', e2'), t2
  | Var(x) when M.mem x !Env.extenv -> Var(x), (M.find x !Env.extenv)
  | Var(x) -> Var(x), (M.find x venv)
  | Constr(x, []) -> Constr(x, []), 
    (match M.find x tycons with
    | Type.TyFun(xs, t) -> t
    | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false)
  | Constr(x, es) -> 
      let ets' = List.map (g env) es in
      begin
        match M.find x tycons with
        | Type.TyFun(xs, (Type.App(Type.Arrow, ys) as t)) -> 
            let t' = Typing.subst env (List.fold_left2 (fun env y (_, t) -> M.add_list (subst_map y t) env) M.empty (L.init ys) ets') t in
            let r', ys' = match t' with Type.App(Type.Arrow, ys') -> L.last ys', ys' | _ -> assert false in
            (unwrap (Constr(x, List.map2 wrap ets' (L.init ys)), (L.last ys)) r'), r'
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_tycon t); assert false
      end
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      let e1', _ = g { env with Env.venv = M.add_list yts (M.add x t venv) } e1 in
      let e2', t2 = g (Env.add_var_type env x t) e2 in
      LetRec({ name = (x, t);
               args = yts;
               body = e1' },
             e2'), t2
  | App(e, es) -> 
      let e', t = g env e in
      let ets' = List.map (g env) es in 
      (match t with 
      | Type.App(Type.Arrow, ys) ->
          let t' = Typing.subst env (List.fold_left2 (fun env y (_, t) -> M.add_list (subst_map y t) env) M.empty (L.init ys) ets') t in
          let r', ys' = match t' with Type.App(Type.Arrow, ys') -> L.last ys', ys' | _ -> assert false in
          (unwrap (App(e', List.map2 wrap ets' (L.init ys)), (L.last ys)) r'), r'
      | _ -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false)
  | WrapBody _ | UnwrapBody _ -> Printf.eprintf "impossible.\n"; assert false 
  | Cons _ | Nil _ -> Printf.eprintf "not implemented.\n"; assert false 
    
let f' env e = 
  let e', _ = g env e in
  e'
    
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
