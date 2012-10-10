(* give names to intermediate values (K-normalization) *)
(* 変換後のコードをなるべくオリジナルに近いものにするため、実際にはほとんどK正規形にはしない。 *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Nil of Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t 
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
and e =
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
  | ExtFunApp of Id.t * e list
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of (Id.t * Type.tycon)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef

let rec ocaml_of_pattern =
  function
  | PtBool(b) -> string_of_bool b
  | PtInt(n) -> string_of_int n
  | PtVar(x, t) -> x
  | PtTuple(ps) -> String.concat ", " (List.map ocaml_of_pattern ps)
  | PtField(xps) -> String.concat ", " (List.map (fun (x, p) -> x ^ " = " ^ (ocaml_of_pattern p)) xps)
  | PtConstr(x, ps) -> x ^ ", " ^ String.concat ", " (List.map ocaml_of_pattern ps)

let rec ocaml_of_e = 
  function
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | Record(xes) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (ocaml_of_e e)) xes)) ^ "}"
  | Field(e, x) -> (ocaml_of_e e) ^ "." ^ x
  | Tuple(es) -> "(" ^ (String.concat ", " (List.map ocaml_of_e es)) ^ ")"
  | Not(e) -> "not " ^ (ocaml_of_e e)
  | And(e1, e2) -> (ocaml_of_e e1) ^ " && " ^ (ocaml_of_e e2)
  | Or(e1, e2) -> (ocaml_of_e e1) ^ " || " ^ (ocaml_of_e e2)
  | Neg(e) -> "! " ^ (ocaml_of_e e)
  | Add(e1, e2) -> (ocaml_of_e e1) ^ " + " ^ (ocaml_of_e e2)
  | Sub(e1, e2) -> (ocaml_of_e e1) ^ " - " ^ (ocaml_of_e e2)
  | Mul(e1, e2) -> (ocaml_of_e e1) ^ " * " ^ (ocaml_of_e e2)
  | Div(e1, e2) -> (ocaml_of_e e1) ^ " / " ^ (ocaml_of_e e2)
  | Var(x) -> x
  | Constr(x, []) -> x
  | Constr(x, es) -> "(" ^ x ^ (String.concat ", " (List.map ocaml_of_e es)) ^ ")"
  | Eq(e1, e2) -> (ocaml_of_e e1) ^ " = " ^ (ocaml_of_e e2)
  | LE(e1, e2) -> (ocaml_of_e e1) ^ " <= " ^ (ocaml_of_e e2) 
  | App(e, args) -> "(" ^ (ocaml_of_e e) ^ " " ^ (String.concat " " (List.map ocaml_of_e args)) ^ ")"
  | ExtFunApp(x, args) -> "(" ^ x ^ " " ^ (String.concat " " (List.map ocaml_of_e args)) ^ ")"
    
let rec ocaml_of_t = 
  function
  | Unit -> "()"
  | Nil _ -> "[]"
  | Exp(e) -> ocaml_of_e e
  | Cons _ -> assert false
  | If(e, e1, e2) -> "if " ^ (ocaml_of_e e) ^ "\n\tthen " ^ (ocaml_of_t e1) ^ "\n\telse " ^ (ocaml_of_t e2)
  | Match(x, pes) -> "match " ^ x ^ " with\n" ^ (String.concat "\n" (List.map (fun (p, e) -> " | " ^ (ocaml_of_pattern p) ^ " -> " ^ (ocaml_of_t e)) pes))
  | Let((s1, t), e1, e2) -> "\nlet " ^ s1 ^ " : " ^ (Type.ocaml_of  t) ^ " = " ^ (ocaml_of_t e1) ^ " in\n" ^ (ocaml_of_t e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "\nlet rec " ^ x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.ocaml_of  t) ^ " =\n"
      ^ (ocaml_of_t e1) ^ " in\n" ^ (ocaml_of_t e2)
  | WrapBody(x, t) -> "(* wrapper " ^ x ^ " " ^ (Type.string_of t) ^ " *)"
  | UnwrapBody(x, t) -> "(* unwrapper " ^ x ^ " " ^ (Type.string_of t) ^ " *)"

let rec insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Exp(e) -> k (e, t)
  | LetRec(fundef, e) ->
      let e', t' = insert_let (e, t) k in
      LetRec(fundef, e'), t'
  | _ ->
      let x = Id.gentmp (Type.prefix t) in
      let e', t' = k (Var(x), t) in
      Let((x, t), e, e'), t'

let rec h env (p, t) = 
  match p, t with
  | Syntax.PtBool(b), _ -> env, (PtBool(b))
  | Syntax.PtInt(n), _ -> env, (PtInt(n))
  | Syntax.PtVar(x, t), _ -> Env.add_var_type env x t, (PtVar(x, t))
  | Syntax.PtTuple(ps), t -> 
      begin
        match t with
        | Type.App(Type.Tuple, ts) -> 
            let env', ps' = 
              List.fold_left 
                (fun (env, ps) (p, t) -> 
                  let env', p = h env (p, t) in
                  (env', p :: ps)) 
                (env, []) (List.combine ps ts) in
            env', PtTuple(ps')
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
  | Syntax.PtField(xps), t -> 
      begin
        match t with
        | Type.App(Type.Record(_, ys), ts) -> 
            let env', xps' = 
              List.fold_left 
                (fun (env, xps) ((x, p), t) -> 
                  let env', p = h env (p, t) in
                  (env', (x, p) :: xps)) 
                (env, []) (List.combine xps ts) in
            env', PtField(xps')
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
  | Syntax.PtConstr(x, []), t -> Env.add_var_type env x t, (PtConstr(x, []))
  | Syntax.PtConstr(x, ps), t -> 
      begin
        match t with
        | Type.Variant(_, ytss) ->
            let env', ps' = 
              List.fold_left 
                (fun (env, ps) (p, t) -> 
                  let env', p = h env (p, t) in
                  (env', p :: ps)) 
                (env, []) (List.combine ps (snd (List.find (fun (y, _) -> x = y) ytss))) in
            env', PtConstr(x, ps')
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
        
let rec g ({ Env.venv = venv; types = types; tycons = tycons } as env) e = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  let _ = D.printf "kNormal.g %s\n" (Syntax.string_of_exp e) in  
  let insert_lets es k =
    let rec insert_lets' es k args =
      match es with
      | [] -> k args
      | (e::es') -> insert_let (g env e) (fun et' -> insert_lets' es' k (args @ [et'])) in
    insert_lets' es k [] in
  let binop e1 e2 f t =
    insert_let (g env e1)
      (fun (e1', _) -> insert_let (g env e2)
        (fun (e2', _) -> f e1' e2', t)) in
  match e with
  | Syntax.Unit -> Unit, Type.App(Type.Unit, [])
  | Syntax.Nil(t) -> Nil(t), t
  | Syntax.Bool(b) -> Exp(Bool(b)), Type.App(Type.Bool, [])
  | Syntax.Int(i) -> Exp(Int(i)), Type.App(Type.Int, [])
  | Syntax.Record(xes) ->
      insert_lets (List.map snd xes) (fun ets' -> Exp(Record(List.combine (List.map fst xes) (List.map fst ets'))), 
        (match M.find (fst (List.hd xes)) types with
        | Type.Poly(_, Type.Field(tid, _)) -> 
            (Type.App(M.find tid tycons, []))
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false))
  | Syntax.Field(e, x) -> 
      let e', t = g env e in 
      insert_let (e', t) (fun (e, t) -> Exp(Field(e, x)), 
      (match t with 
      | Type.App(Type.Record(_, xs), ts) -> List.assoc x (List.combine xs ts)
      | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of t); assert false))
  | Syntax.Tuple(es) -> insert_lets es (fun ets' -> Exp(Tuple(List.map fst ets')), Type.App(Type.Tuple, List.map snd ets'))
  | Syntax.Not(e) -> insert_let (g env e) (fun (e, _) -> Exp(Not(e)), Type.App(Type.Bool, []))
  | Syntax.And(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(And(e1', e2'))) (Type.App(Type.Bool, []))
  | Syntax.Or(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Or(e1', e2'))) (Type.App(Type.Bool, []))
  | Syntax.Neg(e) -> insert_let (g env e) (fun (e, _) -> Exp(Neg(e)), Type.App(Type.Int, []))
  | Syntax.Add(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Add(e1', e2'))) (Type.App(Type.Int, [])) (* 足し算のK正規化 (caml2html: knormal_add) *)
  | Syntax.Sub(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Sub(e1', e2'))) (Type.App(Type.Int, [])) 
  | Syntax.Mul(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Mul(e1', e2'))) (Type.App(Type.Int, [])) 
  | Syntax.Div(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Div(e1', e2'))) (Type.App(Type.Int, [])) 
  | Syntax.Cons(e1, e2) -> assert false
  | Syntax.Var(x) -> Exp(Var(x)), M.find x venv
  | Syntax.Constr(x, []) -> Exp(Constr(x, [])), 
    (match M.find x tycons with
(*    | Type.TyFun(xs, ((Type.App(Type.Int, [])) as t)) -> t *)
    | Type.TyFun(xs, t) -> t 
    | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false)
  | Syntax.Constr(x, es) -> 
      insert_lets es (fun ets' -> Exp(Constr(x, (List.map fst ets'))), 
        (match M.find x tycons with
        | Type.TyFun(xs, (Type.App(Type.Arrow, ys) as t)) -> 
            let xs' = List.map (fun _ -> Type.Meta(Type.newmetavar ())) xs in
            let t' = Typing.subst env (M.add_list2 xs xs' M.empty) t in
            t (* TODO : think again. this type never be used now. why not ? *)
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_tycon t); assert false))
  | Syntax.Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Eq(e1', e2'))) (Type.App(Type.Bool, []))
  | Syntax.LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(LE(e1', e2'))) (Type.App(Type.Bool, []))
  | Syntax.If(e1, e2, e3) -> 
      let e1', t1 = g env e1 in
      let e2', t2 = g env e2 in
      let e3', t3 = g env e3 in
      insert_let (e1', t1) (fun (x, _) -> If(x, e2', e3'), t3)
  | Syntax.Match(Syntax.Var(x), pes) ->
      let pets = List.map 
        (fun (p, e) -> 
          let env', p' = h env (p, (M.find x venv)) in
          let e', t = g env' e in 
          (p', e'), t)
        pes in
      Match(x, List.map fst pets), snd (List.hd pets)
  | Syntax.Match(e, pes) ->
      let e', t = g env e in
      let pets = List.map 
        (fun (p, e) -> 
          let env', p' = h env (p, t) in
          let e', t = g env' e in 
          (p', e'), t)
        pes in
      let x = Id.gentmp (Type.prefix t) in
      Let((x, t), e', (Match(x, List.map fst pets))), snd (List.hd pets)
  | Syntax.LetVar((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (Env.add_var_type env x t) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let venv' = M.add x t venv in
      let e2', t2 = g { env with Env.venv = venv' } e2 in
      let e1', t1 = g { env with Env.venv = M.add_list yts venv' } e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f venv) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Env.extenv with
      | Type.App(Type.Arrow, ts) ->
          let t = List.hd (List.rev ts) in
          let rec bind xs = function (* "xs" are identifiers for the arguments *)
          | [] -> Exp(ExtFunApp(f, xs)), t
          | e2 :: e2s ->
              insert_let (g env e2)
                (fun (x, _) -> bind (xs @ [x]) e2s) in
          (bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
      | _, Type.App(Type.Arrow, ts) as g_e1 ->
          let t = List.hd (List.rev ts) in
          insert_let g_e1
            (fun (f, _) ->
              let rec bind xs = function (* "xs" are identifiers for the arguments *)
              | [] -> Exp(App(f, xs)), t
              | e2 :: e2s ->
                  insert_let (g env e2)
                    (fun (x, _) -> bind (xs @ [x]) e2s) in
              bind [] e2s) (* left-to-right evaluation *)
      | _, t -> Printf.eprintf "type is %s\n" (Type.string_of t); assert false)
  | Syntax.WrapBody(x, t) -> WrapBody(x, t), t
  | Syntax.UnwrapBody(x, t) -> UnwrapBody(x, t), t
        
let fold f env defs = 
  let _, defs' = List.fold_left f (env, []) defs in
  List.rev defs'

let map f defs =
  let f' (({ Env.venv = venv; types = types; tycons = tycons } as env), defs) def =
    let env', def' = 
      match def with 
      | TypeDef(x, t) -> 
          let env' = 
            { env with 
              Env.types  = M.add_list (Type.types t) types; 
              Env.tycons = M.add_list ((x, t) :: (Type.tycons t)) tycons } in
          env', f env' def
      | VarDef((x, t), e) ->  
          Env.add_var_type env x t, f env def
      | RecDef({ name = (x, t); args = yts; body = e1 }) -> 
          let env' = Env.add_var_type env x t in
          env', f env' def in
    env', (def' :: defs) in
  fold f' Env.empty defs

let f' env e = fst (g env e)

let f = 
  Syntax.fold (fun (env, defs) def -> 
    match def with
    | Syntax.TypeDef(x, t) -> TypeDef(x, t) :: defs
    | Syntax.VarDef((x, t), e) -> VarDef((x, t), f' env e) :: defs
    | Syntax.RecDef({ Syntax.name = (x, t); args = yts; body = e }) -> 
        RecDef({ name = (x, t); args = yts; body = f' env e }) :: defs)
    
