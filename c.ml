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
  | And of exp * exp
  | Or of exp * exp
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
  | EnumDef of Id.t list * bool ref
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
    | CType.Enum("", _) -> (string_of_indent depth) ^ "int" ^ (plus x)
    | CType.Enum(name, _) -> (string_of_indent depth) ^ name ^ (plus x)
    | CType.Fun(args, ret) -> (string_of_indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map string_of_type args)) ^ ")" 
    | CType.Struct(tag, xts) -> 
        (string_of_indent depth) ^ "struct " ^ tag ^ " {\n" ^ 
          (String.concat ";\n" (List.map (fun (x, t) -> string_of_type ~x:x ~depth:(depth + 1) t) xts)) ^ ";\n" ^ 
          (string_of_indent depth) ^ "}" ^ (plus x)
    | CType.Union(xts) -> 
        (string_of_indent depth) ^ "union {\n" ^ 
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
  | And(e1, e2) -> "And(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Or(e1, e2) -> (string_of_exp e1) ^ " || " ^ (string_of_exp e2)
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

let rec string_of_statement depth = 
  function
  | Dec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
  | Dec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e)
  | Assign(x, e) -> (string_of_indent depth) ^ (string_of_exp x) ^ " = " ^ (string_of_exp e)
  | Exp(e) -> "Exp(" ^ (string_of_exp e) ^ ")"
  | If(e, s1, s2) -> "If(" ^ (string_of_exp e) ^ ", " ^ (string_of_statement depth s1) ^ " , " ^ (string_of_statement depth s2) ^ ")"
  | Return(e) -> (string_of_indent depth) ^ "return " ^ (string_of_exp e)
  | Seq(s1, s2) -> "Seq(" ^ (string_of_statement depth s1) ^ (string_of_separator s1) ^ ",\n" ^ (string_of_statement depth s2)  ^ ")"
  | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
  | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
and string_of_dec depth = 
  function
  | VarDec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
  | VarDec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e)
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
  | _ -> "" 

let rec string_of_prog (Prog(defs)) =
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
  | s -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 s); assert false
      
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
  Block(List.rev decs, release_boxes decs s')

let wrap_body x t = 
  (Seq(Dec(("p", CType.Box), None), 
       Seq(Assign(Var("p"), CallDir(Var("new_sp"), [Sizeof(t)])),
           Seq(Assign(Deref(Cast(CType.Pointer(t), CallDir(Var("sp_get"), [Var("p")]))), Var(x)),
               Exp(Var("p"))))))
    
let unwrap_body x t = 
  (Exp(Deref(Cast(CType.Pointer(t), CallDir(Var("sp_get"), [Var(x)])))))

let rec concat s1 (x, t) s2 = 
  let _ = D.printf "C.concat x = %s, t = %s, s1 = %s\n" x (CType.string_of t) (string_of_statement 0 s1) in
  match t with  
  | CType.Void -> Seq(s1, s2)
  | t -> 
      begin
        match s1 with
        | Exp(exp) -> Seq(Dec((x, t), Some(exp)), s2)
        | Seq(s1', s2') -> Seq(s1', concat s2' (x, t) s2)
        | _ -> raise (Concat(s1))
      end
      
let toplevel : def list ref = ref []

let failure = CallDir(Var("assert"), [Bool(false)])

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
let rec k tenv t = 
  let _ = D.printf "C.k %s\n" (Type.string_of t) in
  let rec k' local_tenv t =
    match t with
    | Type.Var _ -> CType.Box
    | Type.App(Type.Unit, []) -> CType.Void
    | Type.App(Type.Bool, []) -> CType.Bool
    | Type.App(Type.Int, []) -> CType.Int
    | Type.App(Type.Tuple, ts) -> namety (CType.Struct(Type.name t, List.map (fun t -> let t' = k' local_tenv t in (gentmp t'), t') ts)) (* Tuple型には名前をつける *)
    | Type.App(Type.Arrow, ts) -> namety (CType.Fun(List.map (k' local_tenv) (L.init ts), k' local_tenv (L.last ts))) (* Fun型には名前をつける *)
    | Type.App(Type.Record(x, _), _) when M.mem x tenv -> CType.NameTy(x, { contents = Some(M.find x tenv) }) (* すでに型環境に定義がある場合は名前型を返す *)
    | Type.App(Type.Record(x, _), _) when M.mem x local_tenv -> assert false (* TBD ローカル型環境にある場合は struct タグ名* で参照する *)
    | Type.App(Type.Record(x, ys), ts) -> 
        let r = ref None in
        let t' = CType.Struct(x, List.combine ys (List.map (k' (M.add x (CType.NameTy(x, r)) local_tenv)) ts)) in
        r := Some(t');
        t'
    | Type.App(Type.Variant(x, _), _) when M.mem x tenv -> CType.NameTy(x, { contents = Some(M.find x tenv) })
    | Type.App(Type.Variant(x, ytss), ts) -> 
        let r = ref None in
        let ys = List.map fst ytss in
        let ts' = List.map 
          (fun (y, ts) -> 
            (match ts with
            | [] -> None
            | ts -> Some(CType.Struct(y, List.map (fun t -> let t' = k' (M.add x (CType.NameTy(x, r)) local_tenv) t in gentmp t', t') ts)))) ytss in
        let yts' = List.fold_left2 (fun ts y t -> match t with None -> ts | Some(t) -> (y, t) :: ts) [] ys ts' in
        let t' = 
          match yts' with
          | [] -> CType.Enum("", List.map Id.to_upper ys)
          | yts' -> 
              toplevel := EnumDef(List.map Id.to_upper ys, ref false) :: !toplevel;
              CType.Struct(x, ["type", CType.Int; "u", CType.Union(yts')]) in
        r := Some(t');
        t'
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
      
(* LetをDecに変換 *)
let rec insert_dec (e, t) k = 
  match e with
  | Let(xt, e1, e2) -> 
      let s, t = insert_dec (e2, t) k in 
      Seq(Dec(xt, Some(e1)), s), t
  | e -> k (e, t)
      
(* 文で値を返すために戻り値に代入する文を挿入する *)
let rec insert_assign e s = 
  match s with
  | Exp(exp) when exp = failure -> s
  | Exp(exp) -> Assign(e, exp)
  | If(pred, s1, s2) -> If(pred, insert_assign e s1, insert_assign e s2)
  | Seq(s1, s2) -> Seq(s1, insert_assign e s2)
  | Block(decs, s) -> Block(decs, insert_assign e s)
  | Dec _ | Assign _ | Return _ -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 s); assert false
    
(* if文で値を返すための変換 *)
let rec translate_if (s, t) =
  let rec block_if_body =
    function
    | If(pred, s1, (If _ as s2)) -> If(pred, block s1, block_if_body s2)
    | If(pred, s1, s2) -> If(pred, block s1, block s2)  
    | s -> block s in
  match s with
  | If(pred, Exp(e1), Exp(e2)) -> Exp(Cond(pred, e1, e2)), t
  | If _ -> 
      begin
        match t with
        | CType.Void -> block_if_body s, t
        | t -> let x = gentmp t in 
               let s' = block_if_body (insert_assign (Var(x)) s) in
               (Seq(Dec((x, t), None), Seq(s', Exp(Var(x))))), t
      end
  | _ -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 s); assert false
      
let rec pattern env (e, t) p =
  let _ = D.printf "C.pattern %s %s\n" (string_of_exp e) (Closure.string_of_pattern p) in
  let venv, tenv = env in
  match p with
  | Closure.PtBool(b) -> env, (Eq(e, Bool(b))), (Exp(Nop))
  | Closure.PtInt(n) -> env, (Eq(e, Int(n))), (Exp(Nop))
  | Closure.PtVar(x') -> 
      ((M.add x' t venv), tenv), (Bool(true)), (Dec((x', t), Some(e)))
  | Closure.PtTuple(ps) -> 
      begin
        match t with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, xts)) }) -> 
            List.fold_left 
              (fun (env, pred, dec) ((e, t), p) ->
                let env, pred', dec' = pattern env (e, t) p in
                env, (And(pred, pred')), (Seq(dec, dec')))
              (env, Bool(true), Exp(Nop))
              (List.combine (List.map (fun (x, t) -> Field(e, x), t) xts) ps)
        | t -> D.printf "invalid type : %s\n" (string_of_type t); assert false
      end
  | Closure.PtField(xps) -> 
      begin
        match t with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, xts)) }) -> 
            List.fold_left 
              (fun (env, pred, dec) ((e, t), p) ->
                let env, pred', dec' = pattern env (e, t) p in
                env, (And(pred, pred')), (Seq(dec, dec')))
              (env, Bool(true), Exp(Nop))
              (List.combine (List.map (fun (x, t) -> Field(e, x), t) xts) (List.map snd xps))
        | t -> D.printf "invalid type : %s\n" (string_of_type t); assert false
      end
  | Closure.PtConstr(x, []) -> env, (Eq(e, Var(Id.to_upper x))), (Exp(Nop))
  | Closure.PtConstr(x, ps) -> 
      begin
        match t with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, ["type", CType.Int; "u", CType.Union(yts)])) }) ->
            begin
              match List.find (fun (y, _) -> x = y) yts with
              | y, CType.Struct(_, zts) ->
                  List.fold_left 
                    (fun (env, pred, dec) ((e, t), p) ->
                      let env, pred', dec' = pattern env (e, t) p in
                      env, (And(pred, pred')), (Seq(dec, dec')))
                    (env, Eq(Field(e, "type"), Var(Id.to_upper x)), Exp(Nop))
                    (List.combine (List.map (fun (z, t) -> (Field(Field(Field(e, "u"), x), z)), t) zts) ps)
              | y, t -> D.printf "invalid type : %s\n" (string_of_type t); assert false
            end
        | t -> D.printf "invalid type : %s\n" (string_of_type t); assert false
      end
        
let rec g' env ids e = (* C言語の式生成 (caml2html: c_g) *)
  let _ = D.printf "C.g' %s\n" (Closure.string_of_e e) in
  let venv, tenv = env in
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
  | Closure.Tuple(es) -> 
      let ets = List.map (g' env ids) es in 
      let t = namety (CType.Struct("", List.map (fun (_, t) -> (gentmp t), t) ets)) in (* namety 関数で名前付き構造体型をひく *)
      let name, fields = (* フィールド名は namety でひいたものを採用 *)
        match t with 
        | CType.NameTy(n, { contents = Some(CType.Struct(_, xts)) }) -> n, List.map fst xts 
        | _ -> D.printf "invalid type : %s\n" (string_of_type t); assert false in
      Struct(name, List.combine fields (List.map fst ets)), t
  | Closure.Not(e) -> unop e (fun e -> Not(e)) CType.Bool
  | Closure.And(e1, e2) -> binop e1 e2 (fun e1 e2 -> And(e1, e2)) CType.Bool
  | Closure.Or(e1, e2) -> binop e1 e2 (fun e1 e2 -> Or(e1, e2)) CType.Bool
  | Closure.Neg(e) -> unop e (fun e -> Neg(e)) CType.Int
  | Closure.Add(e1, e2) -> binop e1 e2 (fun e1 e2 -> Add(e1, e2)) CType.Int
  | Closure.Sub(e1, e2) -> binop e1 e2 (fun e1 e2 -> Sub(e1, e2)) CType.Int
  | Closure.Mul(e1, e2) -> binop e1 e2 (fun e1 e2 -> Mul(e1, e2)) CType.Int
  | Closure.Div(e1, e2) -> binop e1 e2 (fun e1 e2 -> Div(e1, e2)) CType.Int
  | Closure.Eq(e1, e2) -> binop e1 e2 (fun e1 e2 -> Eq(e1, e2)) CType.Bool
  | Closure.LE(e1, e2) -> binop e1 e2 (fun e1 e2 -> LE(e1, e2)) CType.Bool
  | Closure.Var(x) -> 
      let x = rename ids x in 
      Var(x), (M.find x venv)
  | Closure.Constr(x, []) -> 
      let x = Id.to_upper x in
      Var(x), (M.find x venv)
  | Closure.Constr(x, es) -> 
      let es' = List.map (fun e -> fst (g' env ids e)) es in
      CallDir(Var(x), es'), return_ty (M.find x venv)
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
            CallDir(Var(l), List.map fst xts), (return_ty (M.find l venv))
        | e2 :: e2s ->
            let e2', t' = g' env ids e2 in
            insert_let (e2', t')
              (fun x -> bind (xts @ [(x, t')]) e2s) in
      bind [] ys (* left-to-right evaluation *) 
        
let rec g env ids e = (* C言語の文生成 (caml2html: c_g) *)
  let _ = D.printf "C.g %s\n" (Closure.string_of_exp e) in
  let venv, tenv = env in
  match e with 
  | Closure.Unit -> Exp(Nop), CType.Void
  | Closure.Nil(t) -> assert false (* let t = k tenv t in Exp(Nil(t)), t *)
  | Closure.WrapBody(x, t) -> wrap_body x (k tenv t), CType.Box
  | Closure.UnwrapBody(x, t) -> let t' = k tenv t in unwrap_body x t', t'
  | Closure.Exp(e) -> 
      let e', t = g' env ids e in 
      insert_dec (e', t) (fun (e, t) -> Exp(e), t)
  | Closure.Cons(x, y) -> assert false (* not implemented *)
  | Closure.If(e, e1, e2) -> 
      let e, _ = g' env ids e in
      let (s1, t1) = (g env ids e1) in
      let (s2, t2) = (g env ids e2) in
      insert_dec (e, t1) (fun (e, t) ->
        assert (CType.equal t1 t2);
        translate_if ((If(e, s1, s2)), t))
  | Closure.MATCH(x, pes) ->
      translate_if 
        (List.fold_right
           (fun (p, e) (s, t) -> 
             let env', pred, dec = pattern env (Var(x), (M.find x venv)) p in
             let s', t' = g env' ids e in
             (If(pred, (Seq(dec, s')), s), t'))
           pes ((Exp(failure)), CType.Void))
  | Closure.Let((x, t), e1, e2) -> 
      let x = rename ids x in
      let s1, t1 = g env ids e1 in
      let s2, t2 = g ((M.add x t1 venv), tenv) ids e2 in
      (concat s1 (x, t1) s2), t2
  | Closure.MakeCls((x, _), { Closure.entry = Id.L(l); Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: c_makecls) *)
      let ys = List.map (rename ids) ys in
      let yts = List.map (fun y -> (y, M.find y venv)) ys in
      let t, maker =
        (try
           let t = find_closure_ty yts (M.find l venv) in
           let maker = maker_name t in
           t, maker
         with
           Not_found ->
             let name, t = closure_ty yts (M.find l venv) in (* (x, t) の型 t は Type.Arrow（関数）型なので型環境からトップレベルの型を引いてクロージャ型に変換する *)
             toplevel := TypeDef((name, t), ref false) :: !toplevel;
             let t = CType.NameTy(name, { contents = Some(t) }) in
             let maker, fundef = make_closure t in
             toplevel := fundef :: !toplevel;
             t, maker) in
      let x' = gentmp t in (* クロージャはトップレベル関数と名前が被っているので改名する *)
      let e2', t2 = g ((M.add x' t venv), tenv) (M.add x x' ids) e2 in
      Seq(Dec((x', t), Some(MakeClosure(Id.L(maker), l, yts))), e2'), t2
        
let constructors =
  function
  | CType.Struct(x, ["type", CType.Int; "u", CType.Union(yts')]) as t ->
      let t = CType.NameTy(x, { contents = Some(t) }) in
      List.fold_left (fun constrs (y, t') -> 
        match t' with
        | CType.Struct(_, zts) -> 
            let zts' = List.map (fun (_, t) -> let z' = gentmp t in z', t) zts in
            ((y, (CType.Fun(List.map snd zts, t))),
             Some(FunDef({ name = Id.L(y);
                           args = zts';
                           body = block (insert_return t 
                                           (Exp(Struct(x, ("type", 
                                                           Var(Id.to_upper y)) :: 
                                             (List.combine (List.map (fun (z, _) -> "u." ^ y ^ "." ^ z) zts) (List.map (fun (z', _) -> Var(z')) zts'))))));
                           ret = t; }, ref true))) :: constrs
        | _ -> constrs) [] yts'
  | CType.Enum("", ys) ->
      List.map (fun y -> (y, CType.Int), None) ys
  | CType.Enum(x, ys) as t ->
      List.map (fun y -> (y, CType.NameTy(x, { contents = Some(t) })), None) ys
  | _ -> []

let h env def = (* トップレベル定義の C 言語変換 (caml2html: c_h) *)
  let () = D.printf "C.h %s\n" (Closure.string_of_def def) in
  let venv, tenv = env in
  match def with
  | Closure.FunDef({ Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e }) ->
      let () = D.printf "C.h (Id.L(%s), %s)\n" x (Type.string_of t) in
      let yts = List.map (fun (y, t) -> (y, k tenv t)) yts in
      let zts = List.map (fun (z, t) -> (z, k tenv t)) zts in
      let body, t' = g ((M.add x (k tenv t) (M.add_list yts (M.add_list zts venv))), tenv) M.empty e in 
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
      let def = VarDef((x, k tenv t), fst (g (venv, tenv) M.empty e)) in
      toplevel := def :: !toplevel; 
      (M.add x (k tenv t) venv), tenv
  | Closure.TypeDef(x, t) -> 
      let t' = k tenv t in
      let constrs = constructors t' in
      let def = TypeDef((x, t'), ref false) in
      toplevel := def :: !toplevel;
      toplevel := (List.flatten (List.map (function (_, Some(def)) -> [def] | _ -> []) constrs)) @ !toplevel;
      (M.add_list (List.map fst constrs) venv), (M.add x (CType.NameTy(x, { contents = Some(t') })) tenv)

(* プログラム全体のCコード生成 (caml2html: c_f) *)
let f (Closure.Prog(defs)) =
  let () = D.printf "\nC.f \n%s\n" (String.concat "\n" (List.map Closure.string_of_def defs)) in
  (* 型変換の k で toplevel に TypeDef を追加する可能性があるので、必ず k の結果を let で束縛してから toplevel を評価すること *)
  let venv = (M.map (k M.empty) !Typing.extenv) in
  let env' = List.fold_left h (venv, M.empty) (L.init defs) in
  let e = 
    match (L.last defs) with 
    | Closure.VarDef((x, Type.App(Type.Unit, [])), e) -> e 
    | def -> Printf.eprintf "invalid def : %s\n" (Closure.string_of_def def); assert false in
  let e, _ = g env' M.empty e in
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
