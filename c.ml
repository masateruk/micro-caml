type t = (* C言語の文 *)
  | Dec of (Id.t * CType.t) * expr option
  | Assign of expr * expr
  | Exp of expr
  | If of expr * t * t
  | Return of expr
  | Seq of t * t
  | Block of dec list * t
and dec =
  | VarDec of (Id.t * CType.t) * expr option
and expr = (* C言語の式 *) 
  | Nop
  | Bool of bool
  | Int of int
  | Struct of Id.t * (Id.t * expr) list (* 構造体の型名とフィールドと式のリスト *)
  | FieldDot of expr * Id.t
  | FieldArrow of expr * Id.t
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | LE of expr * expr
  | Cons of Id.t * Id.t
  | Var of Id.t
  | Cond of expr * expr * expr
  | CallDir of expr * expr list
  | Let of (Id.t * CType.t) * expr * expr
  | MakeClosure of Id.l * Id.t * (Id.t * CType.t) list
  | Sizeof of CType.t
  | Ref of expr
  | Deref of expr
  | Cast of CType.t * expr
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
    | CType.Struct(tag, _, xts) -> 
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
    | CType.Nothing -> (string_of_indent depth)
        
let rec string_of_expr = 
  function
  | Nop -> ""
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Struct(x, xes) -> "(" ^ x ^ "){" ^ (String.concat ", " (List.map (fun (x, e) -> "." ^ x ^ " = " ^ (string_of_expr e)) xes)) ^ "}"
  | FieldDot(e, y) -> (string_of_expr e) ^ "." ^ y
  | FieldArrow(e, y) -> (string_of_expr e) ^ "->" ^ y
  | Not(e) -> "!" ^ (string_of_expr e)
  | And(e1, e2) -> "And(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> (string_of_expr e1) ^ " || " ^ (string_of_expr e2)
  | Neg(e) -> "-" ^ (string_of_expr e)
  | Add(e1, e2) -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Sub(e1, e2) -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | Mul(e1, e2) -> (string_of_expr e1) ^ " * " ^ (string_of_expr e2)
  | Div(e1, e2) -> (string_of_expr e1) ^ " / " ^ (string_of_expr e2)
  | Eq(e1, e2) -> (string_of_expr e1) ^ " == " ^ (string_of_expr e2)
  | LE(e1, e2) -> (string_of_expr e1) ^ " <= " ^ (string_of_expr e2)
  | Cons(x, y) -> x ^ " :: " ^ y
  | Var(x) -> x
  | Cond(e, e1, e2) -> (string_of_expr e) ^ " ? " ^ (string_of_expr e1) ^ " : " ^ (string_of_expr e2)
  | CallDir(x, xs) ->  (string_of_expr x) ^ "(" ^ (String.concat ", " (List.map string_of_expr xs)) ^ ")"
  | Let((x, t), e1, e2)  -> "let " ^ (string_of_type t) ^ " " ^ x ^ " = " ^ (string_of_expr e1) ^ " in " ^ (string_of_expr e2)
  | MakeClosure(Id.L(l), x, yts) -> l ^ "(" ^ x ^ ", " ^ (String.concat ", " (List.map fst yts)) ^ ")" 
  | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
  | Ref(e) -> "&" ^ (string_of_expr e)
  | Deref(e) -> "*(" ^ (string_of_expr e) ^ ")"
  | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_expr e)
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
  | Dec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_expr e)
  | Assign(x, e) -> (string_of_indent depth) ^ (string_of_expr x) ^ " = " ^ (string_of_expr e)
  | Exp(e) -> "Exp(" ^ (string_of_expr e) ^ ")"
  | If(e, s1, s2) -> "If(" ^ (string_of_expr e) ^ ", " ^ (string_of_statement depth s1) ^ " , " ^ (string_of_statement depth s2) ^ ")"
  | Return(e) -> (string_of_indent depth) ^ "return " ^ (string_of_expr e)
  | Seq(s1, s2) -> "Seq(" ^ (string_of_statement depth s1) ^ (string_of_separator s1) ^ ",\n" ^ (string_of_statement depth s2)  ^ ")"
  | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
  | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
and string_of_dec depth = 
  function
  | VarDec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
  | VarDec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_expr e)
let string_of_def = 
  function
  | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, used) when !used ->
      let t' = string_of_type t in
      let name' = x in
      let args' = String.concat ", " (List.map (fun (y, t) -> (string_of_type ~x:y t)) yts) in
      let body' = string_of_statement 0 s in
      t' ^ " " ^ name' ^ "(" ^ args' ^ ")\n" ^ body' ^ "\n\n"
  | TypeDef((x, t), used) ->
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
  match e2 with
  | CallDir _ when CType.is_ref_pointer t -> Assign(e1, e2)
  | _ when CType.is_ref_pointer t -> Seq(Assign(e1, e2), Exp(CallDir(Var("add_ref"), [e1])))
  | _ -> Assign(e1, e2)
      
let rec insert_return ty = 
  let rec return = 
    function
    | Var(x) as e -> 
        if CType.is_ref_pointer ty && not !enable_gc then 
        Seq(Exp(CallDir(Var("add_ref"), [e])), Return(e)) 
        else 
        Return(e)
    | e -> let x = gentmp ty in 
           Seq(Dec((x, ty), Some(e)), return (Var(x))) in
  function
  | Seq(s1, s2) -> Seq(s1, insert_return ty s2)
  | Assign(e1, e2) as s-> Seq(s, return e1)
  | Exp(e) -> return e
  | If(e, s1, s2) -> If(e, insert_return ty s1, insert_return ty s2)
  | Block(decs, s') -> Block(decs, insert_return ty s')
  | s -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 s); assert false
      
let block stm =
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
        | VarDec((x, t), _) when CType.is_ref_pointer t -> insert_release x s
        | _ -> s)
      s decs in
  let decs, s' = collect_decs stm in
  Block(List.rev decs, release_boxes decs s')

let wrap_body x t = 
  if CType.is_ref_pointer t then
  (Exp(Cast(CType.Box, Var(x))))
  else
  (Seq(Dec(("p", CType.Box), None), 
       Seq(Assign(Var("p"), CallDir(Var("new_box"), [Sizeof(t)])),
           Seq(Assign(Deref(Cast(CType.Pointer(t), CallDir(Var("sp_get"), [Var("p")]))), Var(x)),
               Exp(Var("p"))))))
    
    
let unwrap_body x t = 
  if CType.is_ref_pointer t then
  (Exp(Cast(t, Var(x))))
  else
  (Exp(Deref(Cast(CType.Pointer(t), CallDir(Var("sp_get"), [Var(x)])))))

let rec concat s1 (x, t) s2 = 
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

let predef_tenv = M.add_list [
  ("list", CType.list_body);
] M.empty

let predef_venv = M.add_list [
  ("Nil",          CType.Fun([], CType.Pointer(CType.list)));
  ("Cons",         CType.Fun([CType.Box; CType.Pointer(CType.list)], CType.Pointer(CType.list)));
] M.empty

let find_name_ty ty = 
  let find_predef t =
    M.choose (M.filter (fun _ ty -> CType.equal t ty) predef_tenv) in
  let find_typedef t = 
    List.find (function TypeDef((x, t'), _) -> CType.equal t t' | _ -> false) !toplevel in
  try 
    let name, ty = find_predef ty in
    CType.NameTy(name, { contents = Some(ty) })
  with
    Not_found ->
      match find_typedef ty with
      | TypeDef((name, t), _) -> CType.NameTy(name, { contents = Some(t) })
      | _ -> assert false

let find_struct_by_field x =
  let find_predef x =
    M.choose (M.filter (fun _ ty -> match ty with CType.Struct(_, _, xts) -> List.mem_assoc x xts | _ -> false) predef_tenv) in
  try 
    let _, ty = find_predef x in ty
  with
    Not_found ->
      let def = List.find (function TypeDef((_, CType.Struct(_, _, xts)), _) -> List.mem_assoc x xts | _ -> false) !toplevel in
      match def with
      | TypeDef((_, (CType.Struct(_, _, xts) as t)), _) -> t
      | _ -> assert false  

let field_type ty x =
  match ty with
  | CType.Struct(_, _, xts) -> List.assoc x xts
  | t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false 
      
let name_ty ty =
  try
    find_name_ty ty
  with
    Not_found ->
      let name = (gentmp ty) ^ "_t" in
      toplevel := TypeDef((name, ty), ref false) :: !toplevel;
      CType.NameTy(name, { contents = Some(ty) })
        
let rec return_type = 
  function
  | CType.Fun(_, ty_r) -> ty_r
  | CType.NameTy(_, { contents = Some(t) }) -> return_type t
  | _ -> assert false
      
(* 型変換 *)
let rec translate_type tenv ty = 
  let _ = D.printf "C.translate_type %s\n" (Type.string_of_t ty) in
  let kinds = Hashtbl.create 64 in
  let kind x = try Hashtbl.find kinds x with Not_found -> CType.Normal in
  let rec trans_ty reached ty =
    match ty with
    | Type.Var _ -> CType.Box
    | Type.App(Type.Unit, []) -> CType.Void
    | Type.App(Type.Bool, []) -> CType.Bool
    | Type.App(Type.Int, []) -> CType.Int
    | Type.App(Type.Tuple, ts) -> 
        let name = Type.name ty in
        name_ty (CType.Struct(name, kind name, List.map (fun t -> let t' = trans_ty reached t in (gentmp t'), t') ts)) (* Tuple型には名前をつける *)
    | Type.App(Type.Arrow, ts) -> name_ty (CType.Fun(List.map (trans_ty reached) (L.init ts), trans_ty reached (L.last ts))) (* Fun型には名前をつける *)
    | Type.App(Type.Record(x, _), _) when M.mem x tenv -> 
        CType.NameTy(x, { contents = Some(M.find x tenv) }) (* すでに型環境に定義がある場合は名前型を返す *)
    | Type.App(Type.Record(x, _), _) when M.mem x reached -> assert false (* TBD ローカル型環境にある場合は struct タグ名* で参照する *)
    | Type.App(Type.Record(x, ys), ts) -> 
        let r = ref None in
        let ty' = CType.Struct(x, kind x, List.combine ys (List.map (trans_ty (M.add x (CType.NameTy(x, r)) reached)) ts)) in
        r := Some(ty');
        ty'
    | Type.App(Type.Variant(x, _), _) when M.mem x tenv -> 
        begin
          match M.find x tenv with
          | CType.NameTy(_, { contents = Some(CType.Struct(_, CType.Ref, _)) })
          | CType.Struct(_, CType.Ref, _) as t -> CType.Pointer(CType.NameTy(x, { contents = Some(t) }))
          | t -> CType.NameTy(x, { contents = Some(t) })
        end
    | Type.App(Type.Variant(x, _), _) when M.mem x reached -> 
        Hashtbl.add kinds x CType.Ref;
        CType.Pointer(CType.NameTy(x, { contents = Some(M.find x reached) }))
    | Type.App(Type.Variant(x, constrs), _) -> 
        let r = ref None in
        let ys = List.map fst constrs in
        let ts' = List.map 
          (function 
          | _, [] -> None
          | y, ts -> Some(CType.Struct(y, kind y, List.map (fun t -> let t' = trans_ty (M.add x (CType.NameTy(x, r)) reached) t in gentmp t', t') ts))) constrs in
        let yts' = List.fold_left2 (fun ts y t -> match t with None -> (y, CType.Nothing) :: ts | Some(t) -> (y, t) :: ts) [] ys ts' in
        let ty' = 
          if (List.for_all (function _, CType.Nothing -> true | _, _ -> false) yts')
          then CType.Enum("", List.map Id.to_upper ys)
          else (let def = EnumDef(List.map Id.to_upper ys, ref false) in 
                toplevel := def :: !toplevel;
                CType.Struct(x, kind x, ["type", CType.Int; "u", CType.Union(yts')])) in
        r := Some(ty');
        ty'
    | Type.Poly(_, t) -> trans_ty reached t
    | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false in
  trans_ty M.empty ty

let rec translate_tycon tenv = 
  function
  | Type.TyFun(_, ty) -> translate_type tenv ty (* 型変数が残っていてもBoxに変換するため、型引数は無視できる *)
  | t -> Printf.eprintf "invalid type constructor : t = %s\n" (Type.string_of_tycon t); assert false
      
(* 改名マップに ID.t があれば改名後の名前を返す。なければ、元の名前を返す。*)      
let getid m x = if M.mem x m then M.find x m else x
  
(* クロージャ変換補助関数群 *)
let rec closure_type ty args = 
  try
    find_name_ty (CType.Struct("", CType.Closure, ("_f", ty) :: args))
  with
    Not_found ->
      match ty with
      | CType.Fun _ ->
          let name = (Id.genid "closure") ^ "_t" in 
          let t = CType.Struct("", CType.Closure, ("_f", ty) :: args) in
          toplevel := TypeDef((name, t), ref false) :: !toplevel;
          CType.NameTy(name, { contents = Some(t) })
      | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type ty); assert false
    

let make_closure ty = 
  let name = "make" ^ (string_of_type ty) in
  
  let find_make_closure t =
    let _ = List.find (function FunDef({ name = Id.L(x); }, _) when x = name -> true | _ -> false) !toplevel in
    name in
  
  try
    find_make_closure ty
  with
    Not_found ->
      let args, body = 
        match ty with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, CType.Closure, xts)) }) -> 
            let c = gentmp ty in
            let s' = List.fold_right (fun (x, t) s -> (Seq(Assign(FieldDot((Var(c), x)), Var(x)), s))) xts (Exp(Var(c))) in
            xts, Seq(Dec((c, ty), None), s')
        | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type ty); assert false in
      let fundef = FunDef({ name = Id.L(name); args = args; body = block (insert_return ty body); ret = ty }, ref false) in
      toplevel := fundef :: !toplevel;
      name

let apply_closure ty ty_args =  
  let name = "apply" ^ (string_of_type ty) in
  
  let find_apply_closure t =
    let def = List.find (function FunDef({ name = Id.L(x); }, _) when x = name -> true | _ -> false) !toplevel in
    let t = match def with
    | FunDef({ args = args; ret = ret }, _) -> CType.Fun(List.map snd args, ret)
    | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
    name, t in
  
  try
    find_apply_closure ty
  with
    Not_found ->
      let var = gentmp ty in
      let args = (var, ty) :: (List.map (fun t -> gentmp t, t) ty_args) in
      let (f, ty_f), zts = 
        match ty with 
        | CType.NameTy(_, { contents = Some(CType.Struct(_, CType.Closure, xt::xts)) }) -> (xt, xts) 
        | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type ty); assert false in
      let ty_r = return_type ty_f in
      let body = 
        Exp(CallDir(FieldDot((Var(var), f)), 
                    (List.map (fun (y, _) -> Var(y)) (List.tl args)) @ (List.map (fun (z, _) -> FieldDot(Var(var), z)) zts))) in
      let fundef = FunDef({ name = Id.L(name); args = args; body = block (insert_return ty_r body); ret = ty_r }, ref false) in
      toplevel := fundef :: !toplevel;
      name, CType.Fun(ty_args, ty_r)
        
(* 参照カウンタ使用時、Box型の変数はカウンタ操作が必要なため一時変数に代入する *)
let rec insert_let (expr, ty) k =  
  match expr, ty with
  | Var(x), _ -> k (expr, ty)
  | Let(yt, e1, e2), _ ->
      let e3, t3 = insert_let (e2, ty) k in
      Let(yt, e1, e3), t3
  | e, t when (CType.is_ref_pointer t) && (not !enable_gc) ->
      let x = gentmp t in
      let e2, t2 = k ((Var(x)), t) in
      Let((x, t), e, e2), t2
  | e, t -> k (e, t)
      
(* LetをDecに変換 *)
let rec insert_dec (expr, ty) k = 
  match expr with
  | Let(xt, e1, e2) -> 
      let s, t = insert_dec (e2, ty) k in 
      Seq(Dec(xt, Some(e1)), s), t
  | e -> k (e, ty)
      
(* 文で値を返すために戻り値に代入する文を挿入する *)
let rec insert_assign (expr, ty) stm = 
  match stm with
  | Exp(exp) when exp = failure -> stm
  | Exp(exp) -> assign (expr, ty) exp
  | If(pred, s1, s2) -> If(pred, insert_assign (expr, ty) s1, insert_assign (expr, ty) s2)
  | Seq(s1, s2) -> Seq(s1, insert_assign (expr, ty) s2)
  | Block(decs, s) -> Block(decs, insert_assign (expr, ty) s)
  | Dec _ | Assign _ | Return _ -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 stm); assert false
    
(* if文で値を返すための変換 *)
let rec insert_assign_in_if (stm, ty) =
  let rec block_if_body =
    function
    | If(pred, s1, (If _ as s2)) -> If(pred, block s1, block_if_body s2)
    | If(pred, s1, s2) -> If(pred, block s1, block s2)  
    | s -> block s in
  match stm with
  | If(pred, Exp(e1), Exp(e2)) -> Exp(Cond(pred, e1, e2)), ty
  | If _ -> 
      begin
        match ty with
        | CType.Void -> block_if_body stm, ty
        | t -> let x = gentmp t in 
               let s' = block_if_body (insert_assign (Var(x), t) stm) in
               (Seq(Dec((x, t), None), Seq(s', Exp(Var(x))))), t
      end
  | _ -> Printf.eprintf "invalid statement : %s\n" (string_of_statement 0 stm); assert false

let rec pattern (venv, tenv as env) (expr, ty) p =
  let _ = D.printf "C.pattern (%s, %s) %s\n" (string_of_expr expr) (CType.string_of_t ty) (Closure.string_of_pattern p) in

  (* cast が必要なのはパターンマッチ内でバリアント型の型変数の属性にアクセスするときだけなのでグローバル関数にしない *)
  let cast (e, t) t'=
    match t with
    | t when CType.identical t t' -> e
    | CType.Box -> Deref(Cast(CType.Pointer(t'), CallDir(Var("sp_get"), [e])))
    | t -> Cast(t', e) in

  match p with
  | Closure.PtBool(b) -> env, (Eq(cast (expr, ty) CType.Bool, Bool(b))), (Exp(Nop))
  | Closure.PtInt(n) -> env, (Eq(cast (expr, ty) CType.Int, Int(n))), (Exp(Nop))
  | Closure.PtVar(x', t') -> 
      let t' = translate_type tenv t' in
      (M.add x' t' venv, tenv), (Bool(true)), (Dec((x', t'), Some(cast (expr, ty) t')))
  | Closure.PtTuple(ps) -> 
      begin
        match ty with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, _, xts)) }) -> 
            List.fold_left 
              (fun (env, pred, dec) ((e, t), p) ->
                let env, pred', dec' = pattern env (e, t) p in
                env, (And(pred, pred')), (Seq(dec, dec')))
              (env, Bool(true), Exp(Nop))
              (List.combine (List.map (fun (x, t) -> FieldDot(expr, x), t) xts) ps)
        | t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
      end
  | Closure.PtRecord(xps) -> 
      begin
        match ty with
        | CType.NameTy(_, { contents = Some(CType.Struct(_, _, xts)) }) -> 
            List.fold_left 
              (fun (env, pred, dec) ((e, t), p) ->
                let env, pred', dec' = pattern env (e, t) p in
                env, (And(pred, pred')), (Seq(dec, dec')))
              (env, Bool(true), Exp(Nop))
              (List.combine (List.map (fun (x, t) -> FieldDot(expr, x), t) xts) (List.map snd xps))
        | t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
      end
  | Closure.PtConstr(x, []) when M.mem (Id.to_upper x) venv -> env, (Eq(expr, Var(Id.to_upper x))), (Exp(Nop))
  | Closure.PtConstr(x, []) -> 
      let field e x = match ty with CType.Pointer _ -> FieldArrow(e, x) | _ -> FieldDot(e, x) in
      env, (Eq(field expr "type", Var(Id.to_upper x))), (Exp(Nop))
  | Closure.PtConstr(x, ps) ->
      let field e x = match ty with CType.Pointer _ -> FieldArrow(e, x) | _ -> FieldDot(e, x) in
      let rec pattern_constr =
        function
        | CType.Struct(_, _, ["type", CType.Int; "u", CType.Union(yts)]) ->
            begin
              match List.find (fun (y, _) -> x = y) yts with
              | y, CType.Struct(_, _, zts) ->
                  List.fold_left 
                    (fun (env, pred, dec) ((e, t), p) ->
                      let env, pred', dec' = pattern env (e, t) p in
                      env, (And(pred, pred')), (Seq(dec, dec')))
                    (env, Eq(field expr "type", Var(Id.to_upper x)), Exp(Nop))
                    (List.combine (List.map (fun (z, t) -> (FieldDot(FieldDot(field expr "u", x), z)), t) zts) ps)
              | y, t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
            end
        | CType.NameTy(_, { contents = Some(t) }) -> pattern_constr t
        | CType.Pointer(t) -> pattern_constr t
        | t -> Printf.eprintf "invalid type : %s\n" (CType.string_of_t t); assert false in
      pattern_constr ty

let rec g' (venv, tenv as env) ids (expr, ty) = (* C言語の式生成 (caml2html: c_g) *)
  let _ = D.printf "C.g' %s\n" (Closure.string_of_expr expr) in

  let insert_lets ets k =
    let rec insert_lets' ets k ets' =
      match ets with
      | [] -> k ets'
      | (et::rest) -> insert_let (g' env ids et) 
          (fun et' -> 
            insert_lets' rest k (ets' @ [et'])) in
    insert_lets' ets k [] in

  let unop et f ty =
    insert_let (g' env ids et) (fun (e', _) -> f e', ty) in

  let binop et1 et2 f ty =
    insert_let (g' env ids et1) 
      (fun (e1', _) ->
        insert_let (g' env ids et2) (fun (e2', _) -> f e1' e2', ty)) in

  let expr', ty' = 
  match expr with
  | Closure.Bool(b) -> Bool(b), CType.Bool
  | Closure.Int(n) -> Int(n), CType.Int
  | Closure.Not(et) -> unop et (fun et' -> Not(et')) CType.Bool
  | Closure.Neg(et) -> unop et (fun et' -> Neg(et')) CType.Int
  | Closure.And(et1, et2) -> binop et1 et2 (fun et1' et2' -> And(et1', et2')) CType.Bool
  | Closure.Or (et1, et2) -> binop et1 et2 (fun et1' et2' -> Or (et1', et2')) CType.Bool
  | Closure.Add(et1, et2) -> binop et1 et2 (fun et1' et2' -> Add(et1', et2')) CType.Int
  | Closure.Sub(et1, et2) -> binop et1 et2 (fun et1' et2' -> Sub(et1', et2')) CType.Int
  | Closure.Mul(et1, et2) -> binop et1 et2 (fun et1' et2' -> Mul(et1', et2')) CType.Int
  | Closure.Div(et1, et2) -> binop et1 et2 (fun et1' et2' -> Div(et1', et2')) CType.Int
  | Closure.Eq (et1, et2) -> binop et1 et2 (fun et1' et2' -> Eq (et1', et2')) CType.Bool
  | Closure.LE (et1, et2) -> binop et1 et2 (fun et1' et2' -> LE (et1', et2')) CType.Bool
  | Closure.Record(xets) -> 
      insert_lets (List.map snd xets) (fun ets' ->
        let xts', xes' = List.fold_left2 
          (fun (xts', xes') (x, _) (e', t') -> (x, t') :: xts', (x, e') :: xes') ([], []) xets ets' in
        let t = name_ty (CType.Struct("", CType.Normal, List.rev xts')) in
        let name = 
          match t with 
          | CType.NameTy(n, _) -> n 
          | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
        Struct(name, List.rev xes'), t)
  | Closure.Field(et, x) -> 
      insert_let (g' env ids et) (fun (e, t) -> FieldDot(e, x), field_type (find_struct_by_field x) x)
  | Closure.Tuple(ets) -> 
      insert_lets ets (fun ets' -> 
        let t = name_ty (CType.Struct("", CType.Normal, List.map (fun (_, t) -> (gentmp t), t) ets')) in (* name_ty 関数で名前付き構造体型をひく *)
        let name, fields = (* フィールド名は namety でひいたものを採用 *)
          match t with 
          | CType.NameTy(n, { contents = Some(CType.Struct(_, _, xts)) }) -> n, List.map fst xts 
          | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
        Struct(name, List.combine fields (List.map fst ets')), t)
  | Closure.Var(x) -> 
      let x = getid ids x in 
      Var(x), (M.find x venv)
  | Closure.Constr(x, []) when M.mem (Id.to_upper x) venv -> 
      Var(Id.to_upper x), M.find (Id.to_upper x) venv
  | Closure.Constr(x, ets) -> 
      insert_lets ets (fun ets' -> CallDir(Var(x), (List.map fst ets')), return_type (M.find x venv))
  | Closure.App(e, ys) -> 
      let ce = e in
      let rec bind (e, t) ets = 
        function 
        | [] -> 
            begin
              match t with
              | CType.NameTy(_, { contents = Some(CType.Struct(_)) }) -> 
                  let x, t = apply_closure t (List.map snd ets) in
                  CallDir(Var(x), e :: (List.map fst ets)), (return_type t)
              | CType.NameTy(_, { contents = Some(CType.Fun(ty_args, ty_r)) })
              | CType.Fun(ty_args, ty_r) ->
                  CallDir(e, List.map2 (fun (e, t) t' -> e) ets ty_args), ty_r
              | t -> Printf.eprintf "invalid type : %s\n  e = %s\n  Closure.e = %s\n" (string_of_type t) (string_of_expr e) (Closure.string_of_typed_expr ce); assert false
            end
        | e2 :: e2s -> insert_let (g' env ids e2) (fun et' -> bind (e, t) (ets @ [et']) e2s) in
      insert_let (g' env ids e) (fun et' -> bind et' [] ys) (* left-to-right evaluation *)
  | Closure.AppDir(Id.L(l), ys) -> 
      let rec bind ets = 
        function 
        | [] ->
            begin
              match (M.find l venv) with
              | CType.NameTy(_, { contents = Some(CType.Fun(ty_args, ty_r)) })
              | CType.Fun(ty_args, ty_r) ->
                  CallDir(Var(l), List.map2 (fun (e, t) t' -> e) ets ty_args), ty_r
              | t -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
            end
        | e2 :: e2s ->
            insert_let (g' env ids e2) (fun et' -> bind (ets @ [et']) e2s) in
      bind [] ys (* left-to-right evaluation *) 
  in
  expr', ty'
        
let rec g (venv, tenv as env) ids (e, t) = (* C言語の文生成 (caml2html: c_g) *)
  let _ = D.printf "C.g %s\n" (Closure.string_of_term e) in
  match e with 
  | Closure.Unit -> Exp(Nop), CType.Void
  | Closure.WrapBody(x, t) -> wrap_body x (translate_type tenv t), CType.Box
  | Closure.UnwrapBody(x, t) -> let t' = translate_type tenv t in unwrap_body x t', t'
  | Closure.Exp(e) -> 
      insert_dec (g' env ids e) (fun (e', t') -> Exp(e'), t')
  | Closure.If(e, e1, e2) -> 
      insert_dec (g' env ids e) (fun et' ->
        let (s1, t1) = (g env ids e1) in
        let (s2, t2) = (g env ids e2) in
        assert (CType.identical t1 t2);
        insert_assign_in_if (If(fst et', s1, s2), t1))
  | Closure.Match(x, pes) ->
      insert_assign_in_if 
        (List.fold_right
           (fun (p, e) (s, t) -> 
             let env', pred, dec = pattern env (Var(x), (M.find x venv)) p in
             let s', t' = g env' ids e in
             (If(pred, (Seq(dec, s')), s), t'))
           pes ((Exp(failure)), CType.Void))
  | Closure.Let((x, t), et1, et2) -> 
      let x = getid ids x in
      let s1, t1 = g env ids et1 in
      let s2, t2 = g ((M.add x t1 venv), tenv) ids et2 in
      (concat s1 (x, t1) s2), t2
  | Closure.MakeCls((x, _), { Closure.entry = Id.L(l); Closure.actual_fv = ys }, et2) -> (* クロージャの生成 (caml2html: c_makecls) *)
      let ys = List.map (getid ids) ys in
      let yts = List.map (fun y -> (y, M.find y venv)) ys in
      let ty_c = closure_type (M.find l venv) yts in
      let name = make_closure ty_c in
      let x' = gentmp ty_c in (* クロージャはトップレベル関数と名前が被っているので改名する *)
      let e2', t2 = g ((M.add x' ty_c venv), tenv) (M.add x x' ids) et2 in
      Seq(Dec((x', ty_c), Some(MakeClosure(Id.L(name), l, yts))), e2'), t2

let destructor =
  function
  | CType.Struct(x, CType.Ref, ["type", CType.Int; "u", CType.Union(yts)]) as t ->
      let pt = CType.Pointer(CType.NameTy(x, { contents = Some(t) })) in
      [FunDef({ name = Id.L("destruct_" ^ x);
                args = [("base", CType.Pointer(CType.NameTy("ref_base_t", { contents = None })))];
                body = (let p = "p" in
                        let dec = VarDec((p, pt), Some(Cast(pt, Var("base")))) in
                        let s = List.fold_left 
                          (fun s (y, t) -> 
                            let release_fields = 
                              match t with
                              | CType.Struct(_, _, zts) ->
                                  List.fold_left 
                                    (fun s (z, t) ->
                                      if CType.is_ref_pointer t then (Seq(s, Exp(CallDir(Var("release"), [FieldArrow(Var(p), "u." ^ y ^ "." ^ z)])))) else s)
                                    (Exp(Nop)) zts
                              | CType.Nothing -> Exp(Nop) 
                              | _ -> Printf.eprintf "invalid type : %s\n" (CType.string_of_t t); assert false in
                            If(Eq(FieldArrow(Var(p), "type"), Var(Id.to_upper y)), block release_fields, s)) (Block([], (Exp(failure)))) yts in
                        (Block([dec], s)));
                ret = CType.Void; }, ref true)]
  | CType.Struct(x, CType.Ref, yts) as t ->
      let pt = CType.Pointer(CType.NameTy(x, { contents = Some(t) })) in
      [FunDef({ name = Id.L("destruct_" ^ x);
                args = [("base", CType.Pointer(CType.NameTy("ref_base_t", { contents = None })))];
                body = (let p = "p" in
                        let dec = VarDec((p, pt), Some(Cast(pt, Var("base")))) in
                        let s = List.fold_left 
                          (fun s (y, t) -> 
                            if CType.is_ref_pointer t then Seq(s, Exp(CallDir(Var("release"), [FieldArrow(Var(p), y)]))) else s) (Exp(Nop)) yts in
                        (Block([dec], s)));
                ret = CType.Void; }, ref true)]
  | _ -> []
  
let constructors =
  function
  | CType.Struct(x, kind, ["type", CType.Int; "u", CType.Union(yts')]) as t ->
      let t = CType.NameTy(x, { contents = Some(t) }) in
      List.fold_left 
        (fun (constrs, defs) (y, t') -> 
          match kind, t' with
          | CType.Ref, CType.Struct(_, _, zts) -> 
              let pt = CType.Pointer(t) in
              let zts' = List.map (fun (_, t) -> let z' = gentmp t in z', t) zts in
              ((y, (CType.Fun(List.map snd zts, pt))) :: constrs,
               FunDef({ name = Id.L(y);
                        args = zts';
                        body = (let p = "p" in
                                let dec = (VarDec((p, pt), Some(Cast(pt, CallDir(Var("new_ref_base"), [Sizeof(t); Var("destruct_" ^ x)]))))) in
                                let seq = (List.fold_left2 (fun s (z, t) (z', _) -> 
                                  (Seq(s, (assign (FieldArrow(Var(p), "u." ^ y ^ "." ^ z), t) (Var(z')))))) (Assign(FieldArrow(Var(p), "type"), Var(Id.to_upper y))) zts zts') in
                                Block([dec], (Seq(seq, Return(Var(p))))));
                        ret = pt; }, ref true) :: defs)
          | CType.Ref, CType.Nothing -> 
              let pt = CType.Pointer(t) in
              ((y, (CType.Fun([], pt))) :: constrs,
               FunDef({ name = Id.L(y);
                        args = [];
                        body = (let p = "p" in
                                let dec = (VarDec((p, pt), Some(Cast(pt, CallDir(Var("new_ref_base"), [Sizeof(t); Var("destruct_" ^ x)]))))) in
                                let seq = Assign(FieldArrow(Var(p), "type"), Var(Id.to_upper y)) in
                                Block([dec], (Seq(seq, Return(Var(p))))));                                    
                        ret = pt; }, ref true) :: defs)
          | _, CType.Struct(_, _, zts) -> 
              let zts' = List.map (fun (_, t) -> let z' = gentmp t in z', t) zts in
              ((y, (CType.Fun(List.map snd zts, t))) :: constrs,
               FunDef({ name = Id.L(y);
                        args = zts';
                        body = block (insert_return t 
                                        (Exp(Struct(x, ("type", 
                                                        Var(Id.to_upper y)) :: 
                                          (List.combine (List.map (fun (z, _) -> "u." ^ y ^ "." ^ z) zts) (List.map (fun (z', _) -> Var(z')) zts'))))));
                        ret = t; }, ref true) :: defs)
          | _, CType.Nothing -> 
              ((y, (CType.Fun([], t))) :: constrs,
               FunDef({ name = Id.L(y);
                        args = [];
                        body = block (insert_return t 
                                        (Exp(Struct(x, ["type", Var(Id.to_upper y)]))));
                        ret = t; }, ref true) :: defs)
          | _ -> constrs, defs) ([], []) yts'
  | CType.Enum("", ys) ->
      List.map (fun y -> (y, CType.Int)) ys, []
  | CType.Enum(x, ys) as t ->
      List.map (fun y -> (y, CType.NameTy(x, { contents = Some(t) }))) ys, []
  | _ -> [], []

let h (venv, tenv) def = (* トップレベル定義の C 言語変換 (caml2html: c_h) *)
  let () = D.printf "C.h %s\n" (Closure.string_of_def def) in
  match def with
  | Closure.FunDef({ Closure.name = (Id.L(x), ty_f); Closure.args = yts; Closure.formal_fv = zts; Closure.body = et }) ->
      let yts = List.map (fun (y, t) -> (y, translate_type tenv t)) yts in
      let zts = List.map (fun (z, t) -> (z, translate_type tenv t)) zts in
      let body, ty_r = g ((M.add x (translate_type tenv ty_f) (M.add_list yts (M.add_list zts venv))), tenv) M.empty et in 
      begin
        match ty_f with
        | Type.Poly(_, Type.App(Type.Arrow, _))
        | Type.App(Type.Arrow, _) ->
            let body' = block (if (CType.identical ty_r CType.Void) then body else (insert_return ty_r body)) in
            let def = FunDef({ name = Id.L(x); args = yts @ zts; body = body'; ret = ty_r }, ref false) in
            toplevel := def :: !toplevel; 
            (M.add x (CType.Fun(List.map snd (yts @ zts), ty_r)) venv), tenv
        | _ -> Printf.eprintf "invalid type : %s\n" (Type.string_of_t ty_f); assert false
      end
  | Closure.VarDef((x, t), et) -> 
      let def = VarDef((x, translate_type tenv t), fst (g (venv, tenv) M.empty et)) in
      toplevel := def :: !toplevel; 
      (M.add x (translate_type tenv t) venv), tenv
  | Closure.TypeDef(x, t) -> 
      let t' = translate_tycon tenv t in
      let destr = destructor t' in
      let constrs, constr_defs = constructors t' in
      let def = TypeDef((x, t'), ref false) in
      toplevel := def :: !toplevel;
      toplevel := List.rev_append destr !toplevel;
      toplevel := List.rev_append constr_defs !toplevel;
      (M.add_list constrs venv), (M.add x (CType.NameTy(x, { contents = Some(t') })) tenv)

(* プログラム全体のCコード生成 (caml2html: c_f) *)
let f (Closure.Prog(defs)) =
  let () = D.printf "\nC.f \n%s\n" (String.concat "\n" (List.map Closure.string_of_def defs)) in
  (* 型変換の translate_type で toplevel に TypeDef を追加する可能性があるので、必ず translate_type の結果を let で束縛してから toplevel を評価すること *)
  let env = M.fold (fun x t (venv, tenv) -> M.add x (translate_type tenv t) venv, tenv) !Env.extenv.Env.venv (predef_venv, predef_tenv) in
  let env' = List.fold_left h env (L.init defs) in
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
  (Prog(List.rev (main :: !toplevel)))
    
