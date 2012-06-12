type t = (* C言語の文 *)
  | Dec of (Id.t * CType.t) * exp option
  | Assign of exp * exp
  | Exp of exp
  | If of exp * t * t
  | Return of exp
  | Seq of t * t
  | Block of dec list * t
and dec =
  | VarDec of (Id.t * CType.t) * exp option
and exp = (* C言語の式 *) 
  | Nop
  | Nil of CType.t
  | Bool of bool
  | Int of int
  | Struct of Id.t * (Id.t * exp) list (* 構造体の型名とフィールドと式のリスト *)
  | Field of exp * Id.t
  | Not of exp
  | Neg of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Eq of exp * exp
  | LE of exp * exp
  | Cons of Id.t * Id.t
  | Var of Id.t
  | Cond of exp * exp * exp
  | CallDir of exp * exp list
  | Let of (Id.t * CType.t) * exp * exp
  | MakeClosure of Id.l * Id.t * (Id.t * CType.t) list
  | Sizeof of CType.t
  | Ref of exp
  | Deref of exp
  | Cast of CType.t * exp
  | Comma (* いまは演算子の優先順位のためだけに使用している *)
type fundef = { name : Id.l; args : (Id.t * CType.t) list; body : t; ret : CType.t }
type def =
  | VarDef of (Id.t * CType.t) * t
  | FunDef of fundef * bool ref (* used flag *)
  | TypeDef of (Id.t * CType.t) * bool ref
type prog = Prog of def list
    
exception Concat of t
    
let enable_gc = ref false
  
let gentmp t = Id.genid (CType.prefix t)
      
let string_of_indent depth = String.make (depth * 2) ' '

let rec string_of_type ?(x = "") ?(depth = 0) =
  let plus x = if x = "" then "" else " " ^ x in
    function
    | CType.Void -> (string_of_indent depth) ^ "void" 
    | CType.Bool -> (string_of_indent depth) ^ "bool" ^ (plus x)
    | CType.Int -> (string_of_indent depth) ^ "int" ^ (plus x)
    | CType.Fun(args, ret) -> (string_of_indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map string_of_type args)) ^ ")" 
    | CType.Struct(tag, xts) -> 
        (string_of_indent depth) ^ "struct " ^ tag ^ " {\n" ^ 
          (String.concat ";\n" (List.map (fun (x, t) -> string_of_type ~x:x ~depth:(depth + 1) t) xts)) ^ ";\n" ^ 
          (string_of_indent depth) ^ "}" ^ (plus x)
    | CType.NameTy(x', _) -> (string_of_indent depth) ^ x' ^ (plus x)
    | CType.Box -> (string_of_indent depth) ^ "sp_t" ^ (plus x)
    | CType.Pointer t -> (string_of_indent depth) ^ (string_of_type t) ^ "*" ^ (plus x)
        
let rec string_of_exp = 
  function
  | Nop -> ""
  | Nil _ -> "nil"
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Struct(x, xes) -> "(" ^ x ^ "){" ^ (String.concat ", " (List.map (fun (x, e) -> "." ^ x ^ " = " ^ (string_of_exp e)) xes)) ^ "}"
  | Field(e, y) -> (string_of_exp e) ^ "." ^ y
  | Not(e) -> "!" ^ (string_of_exp e)
  | Neg(e) -> "-" ^ (string_of_exp e)
  | Add(e1, e2) -> (string_of_exp e1) ^ " + " ^ (string_of_exp e2)
  | Sub(e1, e2) -> (string_of_exp e1) ^ " - " ^ (string_of_exp e2)
  | Mul(e1, e2) -> (string_of_exp e1) ^ " * " ^ (string_of_exp e2)
  | Div(e1, e2) -> (string_of_exp e1) ^ " / " ^ (string_of_exp e2)
  | Eq(e1, e2) -> (string_of_exp e1) ^ " == " ^ (string_of_exp e2)
  | LE(e1, e2) -> (string_of_exp e1) ^ " <= " ^ (string_of_exp e2)
  | Cons(x, y) -> x ^ " :: " ^ y
  | Var(x) -> x
  | Cond(e, e1, e2) -> (string_of_exp e) ^ " ? " ^ (string_of_exp e1) ^ " : " ^ (string_of_exp e2)
  | CallDir(x, xs) ->  (string_of_exp x) ^ "(" ^ (String.concat ", " (List.map string_of_exp xs)) ^ ")"
  | Let((x, t), e1, e2)  -> "let " ^ (string_of_type t) ^ " " ^ x ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2)
  | MakeClosure(Id.L(l), x, yts) -> l ^ "(" ^ x ^ ", " ^ (String.concat ", " (List.map fst yts)) ^ ")" 
  | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
  | Ref(e) -> "&" ^ (string_of_exp e)
  | Deref(e) -> "*(" ^ (string_of_exp e) ^ ")"
  | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_exp e)
  | Comma -> assert false
      
let rec string_of_separator = 
  function
  | If _ -> ""
  | Seq(_, s) -> string_of_separator s
  | Block _ -> ""
  | _ -> ";"

let rec string_of_prog (Prog(defs)) =
  let rec string_of_statement depth = 
    function
    | Dec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | Dec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e)
    | Assign(x, e) -> (string_of_indent depth) ^ (string_of_exp x) ^ " = " ^ (string_of_exp e)
    | Exp(e) -> (string_of_indent depth) ^ (string_of_exp e) 
    | If(e, s1, s2) -> (string_of_indent depth) ^ "if (" ^ (string_of_exp e) ^ ") " ^ (string_of_statement depth s1) ^ " else " ^ (string_of_statement depth s2)
    | Return(e) -> (string_of_indent depth) ^ "return " ^ (string_of_exp e)
    | Seq(s1, s2) -> (string_of_statement depth s1) ^ (string_of_separator s1) ^ "\n" ^ (string_of_statement depth s2) 
    | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
    | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
  and string_of_dec depth = 
    function
    | VarDec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | VarDec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e) in
  let string_of_def = 
    function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, used) when !used ->
        let t' = string_of_type t in
        let name' = x in
        let args' = String.concat ", " (List.map (fun (y, t) -> (string_of_type ~x:y t)) yts) in
        let body' = string_of_statement 0 s in
          t' ^ " " ^ name' ^ "(" ^ args' ^ ")\n" ^ body' ^ "\n\n"
    | TypeDef((x, t), used) when !used ->
        "typedef " ^ (string_of_type ~x:x t) ^ ";\n\n" 
    | _ -> "" in
    "#include <ucaml.h>\n" ^ 
      (if !enable_gc then "#include <gc.h>\n" else "") ^ 
      "\n" ^ (String.concat "" (List.map string_of_def defs))
      
      
let assign (e1, t) e2 =
  if 
  !enable_gc then Assign(e1, e2)
  else
  match t, e2 with
  | CType.Box, CallDir _ -> Assign(e1, e2)
  | CType.Box, _ -> Seq(Assign(e1, e2), Exp(CallDir(Var("add_ref"), [e1])))
  | _ -> Assign(e1, e2)
      
let rec insert_return t = 
  let rec return = 
    function
    | Var(x) as e -> 
        if t = CType.Box && not !enable_gc then 
        Seq(Exp(CallDir(Var("add_ref"), [e])), Return(e)) 
        else 
        Return(e)
    | e -> let x = gentmp t in 
           Seq(Dec((x, t), Some(e)), return (Var(x))) in
  function
  | Seq(s1, s2) -> Seq(s1, insert_return t s2)
  | Assign(e1, e2) as s-> Seq(s, return e1)
  | Exp(e) -> return e
  | If(e, s1, s2) -> If(e, insert_return t s1, insert_return t s2)
  | Block(decs, s') -> Block(decs, insert_return t s')
  | _ -> assert false
      
let block s =
  let rec collect_decs = 
    function
    | Dec(xt, None) -> [VarDec(xt, None)], Exp(Nop)
    | Dec(xt, Some(e)) -> [VarDec(xt, None)], (assign (Var(fst xt), snd xt) e)
    | Seq(s1, s2) -> 
        let decs1, s1' = collect_decs s1 in
        let decs2, s2' = collect_decs s2 in
        decs2 @ decs1, Seq(s1', s2')
    | s -> [], s in
  let rec remove_nop = 
    function
    | Seq(Exp(Nop), s') -> (remove_nop s')
    | Seq(s', Exp(Nop)) -> (remove_nop s')
    | Seq(s1', s2') -> Seq(remove_nop s1', remove_nop s2')
    | s -> s in
  let release_boxes decs s =
    let rec insert_release x s =
      let e = Exp(CallDir(Var("release"), [Var(x)])) in
      match s with
      | Return _ -> Seq(e, s)
      | Seq(s1, s2) -> Seq(s1, insert_release x s2)
      | s -> Seq(s, e) in    
    if 
    !enable_gc then s
    else 
    List.fold_left 
      (fun s dec -> 
        match dec with 
        | VarDec((x, CType.Box), _) -> insert_release x s
        | _ -> s)
      s decs in
  let decs, s' = collect_decs s in
  Block(List.rev decs, release_boxes decs (remove_nop s'))
    
let rec wrapper (x, t) =
  match t with
  | CType.Fun(args, r) ->
      assert(List.length args = 1);
      let t = List.hd args in
      let y = gentmp t in
      FunDef({ name = Id.L(x);
               args = [(y, t)];
               body = block (insert_return CType.Box (Seq(Dec(("p", CType.Box), None), 
                                                          Seq(Assign(Var("p"), CallDir(Var("new_sp"), [Sizeof(t)])),
                                                              Seq(Assign(Deref(Cast(CType.Pointer(t), CallDir(Var("sp_get"), [Var("p")]))), Var(y)),
                                                                  Exp(Var("p")))))));
               ret = r },
             ref false)
  | CType.NameTy(_, { contents = Some(t) }) -> wrapper (x, t)
  | t -> D.printf "invalid type : %s\n" (string_of_type t); assert false
    
let rec unwrapper (x, t) =
  match t with
  | CType.Fun(args, r) ->
      assert(List.length args = 1);
      let t = List.hd args in
      let y = gentmp t in
      FunDef({ name = Id.L(x);
               args = [(y, t)];
               body = block (insert_return r (Exp(Deref(Cast(CType.Pointer(r), CallDir(Var("sp_get"), [Var(y)]))))));
               ret = r }, 
             ref false)
  | CType.NameTy(_, { contents = Some(t) }) -> unwrapper (x, t)
  | t -> failwith ("invalid type : " ^ (string_of_type t))
      
let rec concat e1 xt e2 = 
  match e1 with
  | Exp(exp) -> 
      (match xt with
      | x, CType.Void -> Seq(e1, e2)
      | xt -> Seq(Dec(xt, Some(exp)), e2))
  | Seq(e1', e2') -> Seq(e1', concat e2' xt e2)
  | _ -> raise (Concat(e1))
      
let toplevel : def list ref = ref []
  
let maker_name t = "_make" ^ (string_of_type t)
let make_closure t = 
  let name = maker_name t in
  let args, body = 
    match t with
    | CType.NameTy(_, { contents = Some(CType.Struct(_, xts)) }) -> 
        let c = gentmp t in
        let s' = List.fold_right (fun (x, t) s -> (Seq(Assign(Field((Var(c), x)), Var(x)), s))) xts (Exp(Var(c))) in
        xts, Seq(Dec((c, t), None), s')
    | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false 
  in
  name, FunDef({ name = Id.L(name); args = args; body = block (insert_return t body); ret = t }, ref false)
    
let applyer_name t = "_apply" ^ (string_of_type t)
let rec apply_closure t args =  
  let x = gentmp t in
  let name = applyer_name t in
  let args = (x, t) :: (List.map (fun t -> gentmp t, t) args) in
  let (f, t'), zts = match t with CType.NameTy(_, { contents = Some(CType.Struct(_, xt::xts)) }) -> (xt, xts) | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
  let ret = match t' with CType.Fun(_, ret) -> ret | _ -> assert false in
  let body = Exp(CallDir(Field((Var(x), f)), (List.map (fun (y, _) -> Var(y)) (List.tl args)) @ (List.map (fun (z, _) -> Field(Var(x), z)) zts))) in
  ret, FunDef({ name = Id.L(name); args = args; body = block (insert_return ret body); ret = ret }, ref false)
    
let find_namety t = 
  let find_typedef t = 
    List.find (fun def -> match def with TypeDef((x, t'), _ )-> CType.equal t t' | _ -> false) !toplevel in
  match find_typedef t with
  | TypeDef((name, t), _) -> CType.NameTy(name, { contents = Some(t) })
  | _ -> assert false
      
let namety t =
  try
    find_namety t
  with
    Not_found ->
      let name = (gentmp t) ^ "_t" in
      toplevel := TypeDef((name, t), ref false) :: !toplevel;
      CType.NameTy(name, { contents = Some(t) })
        
let rec return_ty = 
  function
  | CType.Fun(_, ret) -> ret
  | CType.NameTy(_, { contents = Some(t) }) -> return_ty t
  | _ -> assert false
      
(* 型変換 *)
(*      
let rec k tenv t =
  let _ = D.printf "C.k %s\n" (Type.string_of t) in
  match t with
  | Type.Var _ -> Box
  | Type.App(Type.Unit, []) -> Void
  | Type.App(Type.Bool, []) -> Bool
  | Type.App(Type.Int, []) -> Int
  | Type.App(Type.Arrow, xs) -> namety (Fun(List.map (k tenv) (L.init xs), k tenv (L.last xs))) (* Fun型には名前をつける *)
  | Type.App(Type.Record(x, xs), ys) -> Struct(x, List.combine xs (List.map (k tenv) ys))
  | Type.App(Type.TyFun(_, _), _) -> assert false (* not implemented. is possible ? *)
  | Type.Poly([], t) -> k tenv t
  | Type.Poly _ -> assert false (* not implemented *)
  | Type.Meta _ -> assert false (* not implemented *)
  | Type.NameTy(x, _) when M.mem x tenv -> Pointer(NameTy(x, { contents = Some(M.find x tenv) }))
  | Type.NameTy(x, _) when M.mem x tenv -> NameTy(x, { contents = Some(M.find x tenv) })
  | Type.NameTy(x, { contents = Some(t) }) -> NameTy(x, { contents = Some(k tenv t) })
  | _ -> assert false
*)    
let rec k tenv t = 
  let _ = D.printf "C.k %s\n" (Type.string_of t) in
  let rec k' local_tenv = 
    function
    | Type.Var _ -> CType.Box
    | Type.App(Type.Unit, []) -> CType.Void
    | Type.App(Type.Bool, []) -> CType.Bool
    | Type.App(Type.Int, []) -> CType.Int
    | Type.App(Type.Arrow, xs) -> namety (CType.Fun(List.map (k' local_tenv) (L.init xs), k' local_tenv (L.last xs))) (* Fun型には名前をつける *)
    | Type.App(Type.Record(x, _), _) when M.mem x tenv -> CType.NameTy(x, { contents = Some(M.find x tenv) }) (* すでに型環境に定義がある場合は名前型を返す *)
    | Type.App(Type.Record(x, _), _) when M.mem x local_tenv -> assert false (* TBD ローカル型環境にある場合は struct タグ名* で参照する *)
    | Type.App(Type.Record(x, ys), ts) -> 
        let r = ref None in
        let t' = CType.Struct(x, List.combine ys (List.map (k' (M.add x (CType.NameTy(x, r)) local_tenv)) ts)) in
        r := Some(t');
        t'
(*        
    | Type.App(Type.Record(x, xs), ys) -> Struct(x, List.combine xs (List.map (k' (M.add x tenv) ys))
    | Type.App(Type.Record(f :: fs), []) -> assert false
        begin
          match (M.find f tenv) with
          | Type.Field(t, _) -> namety t
          | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
        end
*)          
    | Type.App(Type.TyFun(_, _), _) -> assert false (* not implemented. is possible ? *)
    | Type.Poly([], t) -> k' local_tenv t
    | Type.Poly _ -> assert false (* not implemented *)
    | Type.Meta _ -> assert false (* not implemented *)
    | Type.NameTy(x, _) when M.mem x local_tenv -> CType.Pointer(CType.NameTy(x, { contents = Some(M.find x local_tenv) }))
    | Type.NameTy(x, _) when M.mem x tenv -> CType.NameTy(x, { contents = Some(M.find x tenv) })
    | Type.NameTy(x, { contents = Some(t) }) -> CType.NameTy(x, { contents = Some(k' local_tenv t) })
    | _ -> assert false in
    k' M.empty t
      
let find_closure_ty yts t = find_namety (CType.Struct("", ("f", t) :: yts))
  
let rec closure_ty yts t = 
  match t with
  | CType.Fun(args, ret) -> 
      let name = (Id.genid "closure") ^ "_t" in name, CType.Struct("", ("f", t) :: yts)
  | CType.NameTy(_, { contents = Some(t') }) -> closure_ty yts t'
  | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
    
let rec split = 
  function
  | Dec _ -> assert false
  | Assign(e, _) as s -> Some(s), e
  | Exp(e) -> None, e
  | If(_, s1, s2) as s -> (match s1, s2 with Assign(e1, _), Assign(e2, _) when e1 = e2 -> Some(s), e1 | _ -> assert false)
  | Return _ -> assert false
  | Seq(s1, s2) -> 
      let s2', e2' = split s2 in
      (match s2' with
      | Some(s2') -> Some(Seq(s1, s2')), e2'
      | None -> Some(s1), e2')
  | Block(_, s) -> split s
      
(* 改名。改名マップに ID.t があれば改名後の名前を返す。なければ、元の名前を返す。*)      
let rename m x = if M.mem x m then M.find x m else x
  
(* 参照カウンタ使用時、Box型の変数はカウンタ操作が必要なため一時変数に代入する *)
let rec insert_let (e, t) k =  
  match e, t with
  | Var(x), _ -> k e
  | Let(yt, e1, e2), _ ->
      let e3, t3 = insert_let (e2, t) k in
      Let(yt, e1, e3), t3
  | e, CType.Box when not !enable_gc ->
      let x = gentmp t in
      let e2, t2 = k (Var(x)) in
      Let((x, t), e, e2), t2
  | e, _ -> k e
      
let rec insert_dec e k = 
  match e with
  | Let(xt, e1, e2) -> Seq(Dec(xt, Some(e1)), insert_dec e2 k)
  | e -> k e
      
let rec g' env ids e = (* C言語の式生成 (caml2html: c_g) *)
  let _ = D.printf "C.g' %s\n" (Closure.string_of_e e) in
  let unop e f ty =
    let e', t = g' env ids e in 
    insert_let (e', t) (fun e -> f e, ty) in
  let binop e1 e2 f ty =
    let e1', t1 = g' env ids e1 in 
    insert_let (e1', t1) 
      (fun e1'' ->
        let e2', t2 = g' env ids e2 in 
        insert_let (e2', t2) (fun e2'' -> f e1'' e2'', ty)) in
  match e with
  | Closure.Bool(b) -> Bool(b), CType.Bool
  | Closure.Int(i) -> Int(i), CType.Int
  | Closure.Record(xes) -> 
      let ets = List.map (fun (x, e) -> g' env ids e) xes in 
      let t = namety (CType.Struct("", List.combine (List.map fst xes) (List.map snd ets))) in
      let name = match t with CType.NameTy(n, _) -> n | _ -> D.printf "invalid type : %s\n" (string_of_type t); assert false in
      Struct(name, List.combine (List.map fst xes) (List.map fst ets)), t
  | Closure.Field(e, x) -> 
      let e', t = g' env ids e in 
      insert_let (e', t) (fun e -> Field(e, x), t)
  | Closure.Not(e) -> unop e (fun e -> Not(e)) CType.Bool
  | Closure.Neg(e) -> unop e (fun e -> Neg(e)) CType.Int
  | Closure.Add(e1, e2) -> binop e1 e2 (fun e1 e2 -> Add(e1, e2)) CType.Int
  | Closure.Sub(e1, e2) -> binop e1 e2 (fun e1 e2 -> Sub(e1, e2)) CType.Int
  | Closure.Mul(e1, e2) -> binop e1 e2 (fun e1 e2 -> Mul(e1, e2)) CType.Int
  | Closure.Div(e1, e2) -> binop e1 e2 (fun e1 e2 -> Div(e1, e2)) CType.Int
  | Closure.Eq(e1, e2) -> binop e1 e2 (fun e1 e2 -> Eq(e1, e2)) CType.Bool
  | Closure.LE(e1, e2) -> binop e1 e2 (fun e1 e2 -> LE(e1, e2)) CType.Bool
  | Closure.Var(x) -> 
      let x = rename ids x in 
      Var(x), (M.find x env)
  | Closure.App(x, ys) -> 
      let rec apply (x, t) = 
        match t with
        | CType.NameTy(_, { contents = Some(CType.Struct(_))}) -> 
            let rec bind xts = 
              function 
              | [] ->
                  let rt, fundef = apply_closure t (List.map snd xts) in
                  toplevel := fundef :: !toplevel;
                  CallDir(Var(applyer_name t), x :: (List.map fst xts)), rt
              | e2 :: e2s ->
                  let e2', t' = g' env ids e2 in
                  insert_let (e2', t')
                    (fun x -> bind (xts @ [(x, t')]) e2s) in
            bind [] ys (* left-to-right evaluation *)
        | CType.NameTy(_, { contents = Some(t)}) -> apply (x, t)
        | CType.Fun(args, rt) ->
            let rec bind xts = 
              function 
              | [] ->
                  assert(List.for_all2 CType.equal (List.map snd xts) args);
                  CallDir(x, List.map fst xts), rt
              | e2 :: e2s ->
                  let e2', t' = g' env ids e2 in
                  insert_let (e2', t')
                    (fun x -> bind (xts @ [(x, t')]) e2s) in
            bind [] ys (* left-to-right evaluation *)
        | t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
      let x', t = (g' env ids x) in
      insert_let (x', t) (fun x -> apply (x, t))
  | Closure.AppDir(Id.L(l), ys) -> 
      let rec bind xts = 
        function 
        | [] ->
            CallDir(Var(l), List.map fst xts), (return_ty (M.find l env))
        | e2 :: e2s ->
            let e2', t' = g' env ids e2 in
            insert_let (e2', t')
              (fun x -> bind (xts @ [(x, t')]) e2s) in
      bind [] ys (* left-to-right evaluation *) 
        
let rec g env ids e = (* C言語の文生成 (caml2html: c_g) *)
  let _ = D.printf "C.g %s\n" (Closure.string_of_exp e) in
  match e with 
  | Closure.Unit -> Exp(Nop), CType.Void
  | Closure.Nil(t) -> assert false (* let t = k tenv t in Exp(Nil(t)), t *)
  | Closure.Exp(e) -> 
      let e', t = g' env ids e in 
      (insert_dec e' (fun e -> Exp(e))), t
  | Closure.Cons(x, y) -> assert false (* not implemented *)
  | Closure.If(e, e1, e2) -> 
      let e, _ = g' env ids e in
      let (s1, t1) = (g env ids e1) in
      let (s2, t2) = (g env ids e2) in
      insert_dec e (fun e ->
        assert (CType.equal t1 t2);
        (match s1, s2 with
        | Exp(e1'), Exp(e2') -> Exp(Cond(e, e1', e2'))
        | _, _ -> 
            let s1', e1' = split s1 in
            let s2', e2' = split s2 in
            (match s1', s2' with 
            | Some(s1'), Some(s2') -> 
                let z = gentmp t1 in 
                Seq(Dec((z, t1), None), Seq(If(e, block (Seq(s1', Assign(Var(z), e1'))), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
            | Some(s1'), None -> 
                let z = gentmp t1 in 
                Seq(Dec((z, t1), None), Seq(If(e, block (Seq(s1', Assign(Var(z), e1'))), block (Assign(Var(z), e2'))), Exp(Var(z))))
            | None, Some(s2') -> 
                let z = gentmp t1 in 
                Seq(Dec((z, t1), None), Seq(If(e, block (Assign(Var(z), e1')), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
            | None, None -> 
                Exp(Cond(e, e1', e2'))))), t1
  | Closure.Let((x, t), e1, e2) -> 
      let x = rename ids x in
      let e1', t1 = g env ids e1 in
      let e2', t2 = g (M.add x t1 env) ids e2 in
      (concat e1' (x, t1) e2'), t2
  | Closure.MakeCls((x, _), { Closure.entry = Id.L(l); Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: c_makecls) *)
      let ys = List.map (rename ids) ys in
      let yts = List.map (fun y -> (y, M.find y env)) ys in
      let t, maker =
        (try
           let t = find_closure_ty yts (M.find l env) in
           let maker = maker_name t in
           t, maker
         with
           Not_found ->
             let name, t = closure_ty yts (M.find l env) in (* (x, t) の型 t は Type.Arrow（関数）型なので型環境からトップレベルの型を引いてクロージャ型に変換する *)
             toplevel := TypeDef((name, t), ref false) :: !toplevel;
             let t = CType.NameTy(name, { contents = Some(t) }) in
             let maker, fundef = make_closure t in
             toplevel := fundef :: !toplevel;
             t, maker) in
      let x' = gentmp t in (* クロージャはトップレベル関数と名前が被っているので改名する *)
      let e2', t2 = g (M.add x' t env) (M.add x x' ids) e2 in
      Seq(Dec((x', t), Some(MakeClosure(Id.L(maker), l, yts))), e2'), t2
        
let h env def = (* トップレベル定義の C 言語変換 (caml2html: c_h) *)
  let () = D.printf "C.h %s\n" (Closure.string_of_def def) in
  let venv, tenv = env in
  match def with
  | Closure.FunDef({ Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e }) ->
      let () = D.printf "C.h (Id.L(%s), %s)\n" x (Type.string_of t) in
      let yts = List.map (fun (y, t) -> (y, k tenv t)) yts in
      let zts = List.map (fun (z, t) -> (z, k tenv t)) zts in
      let body, t' = g (M.add x (k tenv t) (M.add_list yts (M.add_list zts venv))) M.empty e in 
      begin
        match t with
        | Type.App(Type.Arrow, _) ->
            (* 戻り値がクロージャになっているケースがあるので、シグネチャの型ではなく変換後の方を使用する。ただし型環境には変換前の方を入れているので再帰関数については再考が必要 *)
            let body' = block (if (CType.equal t' CType.Void) then body else (insert_return t' body)) in
            let def = FunDef({ name = Id.L(x); args = yts @ zts; body = body'; ret = t' }, ref false) in
            toplevel := def :: !toplevel; 
            (M.add x (CType.Fun(List.map snd (yts @ zts), t')) venv), tenv
        | _ -> Printf.eprintf "invalid type : %s\n" (Type.string_of t); assert false
      end
  | Closure.VarDef((x, t), e) -> 
      let def = VarDef((x, k tenv t), fst (g venv M.empty e)) in
      toplevel := def :: !toplevel; 
      (M.add x (k tenv t) venv), tenv
  | Closure.TypeDef(x, t) -> 
      let t' = k tenv t in
      let def = TypeDef((x, t'), ref false) in
      toplevel := def :: !toplevel; 
      venv, (M.add x (CType.NameTy(x, { contents = Some(t') })) tenv)
        
(* プログラム全体のCコード生成 (caml2html: c_f) *)
let f (Closure.Prog(defs)) =
  let () = D.printf "\nC.f \n%s\n" (String.concat "\n" (List.map Closure.string_of_def defs)) in
  (* 型変換の k で toplevel に TypeDef を追加する可能性があるので、必ず k の結果を let で束縛してから toplevel を評価すること *)
  List.iter (fun (x, t) -> let w = wrapper (x, k M.empty t) in toplevel := w :: !toplevel) (Wrap.wrapper_list ());
  List.iter (fun (x, t) -> let u = unwrapper (x, k M.empty t) in toplevel := u :: !toplevel) (Wrap.unwrapper_list ());
  let venv = (M.map (k M.empty) !Typing.extenv) in
  let env' = List.fold_left h (venv, M.empty) (L.init defs) in
  let e = 
    match (L.last defs) with 
    | Closure.VarDef((x, Type.App(Type.Unit, [])), e) -> e 
    | def -> Printf.eprintf "invalid def : %s\n" (Closure.string_of_def def); assert false in
  let e, _ = g (fst env') M.empty e in
  let main = FunDef({ name = Id.L("main");
                      args = [];
                      body = block (insert_return CType.Int 
                                      (if !enable_gc then
                                        (Seq(Exp(CallDir(Var("GC_init"), [])), (Seq(e, Exp(Int(0))))))
                                       else
                                        (Seq(e, Exp(Int(0))))));
                      ret = CType.Int }, 
                    ref true) 
  in
  (Prog(List.rev !toplevel @ [main]))
