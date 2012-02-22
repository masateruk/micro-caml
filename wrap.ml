open Syntax

module MapSet =
  Map.Make
    (struct
      type t = Type.t
      let compare = compare
    end)
    
let wrappers = ref MapSet.empty
let unwrappers = ref MapSet.empty

let wrapper_list () = List.map snd (MapSet.bindings !wrappers)
let unwrapper_list () = List.map snd (MapSet.bindings !unwrappers)

let new_wrapper t = 
  match t with
    | Type.App(Type.Bool, []) -> "wrap_bool", Type.App(Type.Arrow, [t; Type.Var(Type.newtyvar ())])
    | Type.App(Type.Int, []) -> "wrap_int", Type.App(Type.Arrow, [t; Type.Var(Type.newtyvar ())])
    | Type.App(Type.Arrow, us) -> 
	let us' = [(Type.App(Type.Arrow, (List.map (fun _ -> (Type.Var(Type.newtyvar ()))) us))); (Type.Var(Type.newtyvar ()))] in
	  "wrap_closure" ^ (Id.genid ""), (Type.App(Type.Arrow, us'))
    | _ -> Printf.eprintf "not implemented.\n"; assert false
  
let wrapper t = 
  try
    fst (MapSet.find t !wrappers)
  with
      Not_found ->
	let name, t' = new_wrapper t in
	  wrappers := MapSet.add t (name, t') !wrappers;
	  name

let new_unwrapper t = 
  match t with
    | Type.App(Type.Bool, []) -> "uwrap_bool", Type.App(Type.Arrow, [Type.Var(Type.newtyvar ()); t])
    | Type.App(Type.Int, []) -> "unwrap_int", Type.App(Type.Arrow, [Type.Var(Type.newtyvar ()); t])
    | Type.App(Type.Arrow, us) -> "unwrap_closure" ^ (Id.genid ""), Type.App(Type.Arrow, [Type.Var(Type.newtyvar ());
											  (Type.App(Type.Arrow, (List.map (fun _ -> Type.Var(Type.newtyvar ())) us)))])
    | _ -> Printf.eprintf "not implemented.\n"; assert false
	
let unwrapper t = 
  try
    fst (MapSet.find t !unwrappers)
  with
      Not_found ->
	let name, t' = new_unwrapper t in
	  unwrappers := MapSet.add t (name,  t') !unwrappers;
	  name

let rec has_tyvar t = match t with
  | Type.Var _ -> true
  | Type.App(Type.TyFun(_, s), us) -> (has_tyvar s) && (List.exists has_tyvar us)
  | Type.App(_, us) -> List.exists has_tyvar us
  | Type.Poly(_, s) -> has_tyvar s
  | Type.Meta({ contents = None }) -> false
  | Type.Meta({ contents = Some(t') }) -> has_tyvar t'
  
(* 関数適応時の引数の包み込み。Tiger本の p.345 *)
let rec wrap (e, s) t = 
  let _ = D.printf "Wrap.wrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_exp e) (Type.string_of_typ s) (Type.string_of_typ t) in
    match s, t with
      | Type.Poly([], s), t -> wrap (e, s) t
      | s, Type.Poly([], t) -> wrap (e, s) t
      | Type.Var _, Type.Var _ -> e
      | Type.App(Type.Bool, []), Type.Var _ -> 
	  App(Var(wrapper s), [e])
      | Type.App(Type.Int, []), Type.Var _ -> 
	  App(Var(wrapper s), [e])
      | Type.App(Type.Arrow, us), Type.Var _ -> 
	  let yts = List.map (fun u -> (Id.gentmp u, Type.Var(Type.newtyvar ()))) (L.init us) in
	  let r = Type.Var(Type.newtyvar ()) in
	  let e' = 
	    App(Var(wrapper s), 
		[LetRec({ name = ("fw", Type.App(Type.Arrow, (List.map snd yts) @ [r])); 
			  args = yts;
			  body = wrap (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r },
			Var("fw"))]) in
	  let () = D.printf "e' = %s\n" (ocaml_of_exp e') in
	    e'
      | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar t ->
	  let yts = List.map (fun v -> (Id.gentmp v, v)) (L.init vs) in
	    begin
	      match (L.last vs) with
		| Type.Var _ -> 
		    LetRec({ name = ("fw", Type.App(Type.Arrow, (List.map snd yts) @ [L.last vs])); 
			     args = yts;
			     body = wrap 
			       (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
			   Var("fw"))
		| r -> 
		    LetRec({ name = ("fw", Type.App(Type.Arrow, (List.map snd yts) @ [r])); 
			     args = yts;
			     body = unwrap (App(e, List.map2 (fun (y, t) u -> unwrap (Var(y), t) u) yts (L.init us)), (L.last us)) r }, 
			   Var("fw"))
	    end
      | s, t when s = t -> e
      | _ -> Printf.eprintf "not implemented.\n"; assert false
	  
and unwrap (e, s) t = 
  let _ = D.printf "Wrap.unwrap \n  (e = %s,\n   s = %s)\n  ,t = %s\n" (string_of_exp e) (Type.string_of_typ s) (Type.string_of_typ t) in
    match s, t with
      | Type.Poly([], s), t -> unwrap (e, s) t
      | s, Type.Poly([], t) -> unwrap (e, s) t
      | Type.Var _, Type.Var _ -> e
      | Type.Var _, Type.App(Type.Bool, []) -> 
	  App(Var(unwrapper t), [e])
      | Type.Var _, Type.App(Type.Int, []) -> 
	  App(Var(unwrapper t), [e])
      | Type.Var _, Type.App(Type.Arrow, vs) ->
	  let e' = 
	    let yts = List.map (fun v -> (Id.gentmp v, v)) (L.init vs) in
	      LetRec({ name = ("fu", t);
		       args = yts;
		       body =
			 let t' = List.map (fun _ -> Type.Var(Type.newtyvar ())) vs in
			 let e' = unwrap (e, Type.App(Type.Arrow, t')) t in
			   unwrap (App(e', (List.map2 (fun (y, t) t' -> wrap (Var(y), t) t') yts (L.init t'))), (L.last t')) (L.last vs) },
		     Var("fu")) in
	    e'
      | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar s ->
	    App(Var(unwrapper t), [e])
      | s, t when s = t -> e
      | _ -> Printf.eprintf "not implemented.\n"; assert false


let subst_map s t =
  let _ = D.printf "Wrap.subst_map %s %s\n" (Type.string_of_typ s) (Type.string_of_typ t) in
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
    
let rec g env e =
  let _ = D.printf "Wrap.g %s\n" (string_of_exp e) in
  match e with
  | Unit -> Unit, Type.App(Type.Unit, [])
  | Bool(b) -> Bool(b), Type.App(Type.Bool, [])
  | Int(n) -> Int(n), Type.App(Type.Int, [])
  | Not(e) -> let e', _ = g env e in Not(e'), Type.App(Type.Bool, [])
  | Neg(e) -> let e', _ = g env e in Neg(e'), Type.App(Type.Bool, [])
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
  | Let((x, t), e1, e2) -> 
      let e1', _ = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
	Let((x, t), e1', e2'), t2
  | Var(x) when M.mem x !Typing.extenv -> Var(x), (M.find x !Typing.extenv)
  | Var(x) -> Var(x), (M.find x env)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      let e1', _ = g (M.add_list yts (M.add x t env)) e1 in
      let e2', t2 = g (M.add x t env) e2 in
	LetRec({ name = (x, t);
		 args = yts;
		 body = e1' },
	       e2'), t2
  | App(e, es) -> 
      let e', t = g env e in
      let ets' = List.map (g env) es in	
	(match t with 
	   | Type.App(Type.Arrow, ys) ->
	       let t' = Typing.subst (List.fold_left2 (fun env y (_, t) -> M.add_list (subst_map y t) env) M.empty (L.init ys) ets') t in
	       let r', ys' = match t' with Type.App(Type.Arrow, ys') -> L.last ys', ys' | _ -> assert false in
		 (unwrap (App(e', List.map2 wrap ets' (L.init ys)), (L.last ys)) r'), r'
	   | _ -> Printf.eprintf "invalid type : %s\n" (Type.string_of_typ t); assert false)
  | _ -> Printf.eprintf "not implemented.\n"; assert false    

let f e = 
  let e', _ = g M.empty e in
    MapSet.iter (fun _ (name, t) -> Typing.extenv := M.add name t !Typing.extenv) !wrappers;
    MapSet.iter (fun _ (name, t) -> Typing.extenv := M.add name t !Typing.extenv) !unwrappers;
    e'


