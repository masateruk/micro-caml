type ty =
    | Void 
    | Int
    | Bool
    | Fun of ty list * ty
    | Struct of (Id.t * ty) list
    | NameTy of Id.t * ty
    | Box
    | Pointer of ty
type t = (* C言語の文 *)
    | Dec of (Id.t * ty) * exp option
    | Assign of exp * exp
    | Exp of exp
    | If of exp * t * t
    | Return of exp
    | Seq of t * t
    | Block of dec list * t
and dec =
    | VarDec of (Id.t * ty) * exp option
and exp = (* C言語の式 *) 
    | Nop
    | Nil of ty
    | BoolExp of bool
    | IntExp of int
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
    | Let of (Id.t * ty) * exp * exp
    | MakeClosure of Id.l * Id.t * (Id.t * ty) list
    | Field of Id.t * Id.t
    | Sizeof of ty
    | Ref of exp
    | Deref of exp
    | Cast of ty * exp
    | Comma (* いまは演算子の優先順位のためだけに使用している *)
type fundef = { name : Id.l; args : (Id.t * ty) list; body : t; ret : ty }
type def =
    | FunDef of fundef * bool ref (* used flag *)
    | TypeDef of (Id.t * ty) * bool ref
type prog = Prog of def list

exception Concat of t

let enable_gc = ref false

let gentmp t =
  let rec id_of_ty = function
    | Void -> assert false
    | Int -> "n"
    | Bool -> "b"
    | Fun _ -> "pfn"
    | Struct _ -> "st"
    | NameTy(_, t') -> id_of_ty t' 
    | Box -> "v" 
    | Pointer _ -> assert false in
    Id.genid (id_of_ty t)

let string_of_indent depth = String.make (depth * 2) ' '
(*  
let rec string_of_type = function
  | Void -> "void" 
  | Bool -> "bool"
  | Int -> "int"
  | Fun(args, ret) -> (string_of_type ret) ^ " (*)(" ^ (String.concat ", " (List.map string_of_type args)) ^ ")" 
  | Struct(xts) -> "struct(" ^ (String.concat "; " (List.map (fun (x, t) -> x ^ " : " ^ string_of_type t) xts)) ^ ")"
  | NameTy(x', _) -> x'
  | Box -> "sp_t"
  | Pointer t -> (string_of_type t) ^ "_ptr"
*)
let rec string_of_type ?(x = "") ?(depth = 0) =
  let plus x = if x = "" then "" else " " ^ x in
    function
      | Void -> (string_of_indent depth) ^ "void" 
      | Bool -> (string_of_indent depth) ^ "bool" ^ (plus x)
      | Int -> (string_of_indent depth) ^ "int" ^ (plus x)
      | Fun(args, ret) -> (string_of_indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map string_of_type args)) ^ ")" 
      | Struct(xts) -> 
	  (string_of_indent depth) ^ "struct {\n" ^ (String.concat ";\n" (List.map (fun (x, t) -> string_of_type ~x:x ~depth:(depth + 1) t) xts)) ^ ";\n" ^ (string_of_indent depth) ^ "}" ^ (plus x)
      | NameTy(x', _) -> (string_of_indent depth) ^ x' ^ (plus x)
      | Box -> (string_of_indent depth) ^ "sp_t" ^ (plus x)
      | Pointer t -> (string_of_indent depth) ^ (string_of_type t) ^ "*" ^ (plus x)

let rec string_of_exp = function
  | Nop -> ""
  | Nil _ -> "nil"
  | BoolExp(b) -> string_of_bool b
  | IntExp(i) -> string_of_int i
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
  | Field(x, y) -> x ^ "." ^ y
  | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
  | Ref(e) -> "&" ^ (string_of_exp e)
  | Deref(e) -> "*(" ^ (string_of_exp e) ^ ")"
  | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_exp e)
  | Comma -> assert false

let rec string_of_separator = function
  | If _ -> ""
  | Seq(_, s) -> string_of_separator s
  | Block _ -> ""
  | _ -> ";"

let rec string_of_prog (Prog(defs)) =
  let rec string_of_statement depth = function
    | Dec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | Dec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e)
    | Assign(x, e) -> (string_of_indent depth) ^ (string_of_exp x) ^ " = " ^ (string_of_exp e)
    | Exp(e) -> (string_of_indent depth) ^ (string_of_exp e) 
    | If(e, s1, s2) -> (string_of_indent depth) ^ "if (" ^ (string_of_exp e) ^ ") " ^ (string_of_statement depth s1) ^ " else " ^ (string_of_statement depth s2)
    | Return(e) -> (string_of_indent depth) ^ "return " ^ (string_of_exp e)
    | Seq(s1, s2) -> (string_of_statement depth s1) ^ (string_of_separator s1) ^ "\n" ^ (string_of_statement depth s2) 
    | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
    | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
  and string_of_dec depth = function
    | VarDec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | VarDec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e) in
  let string_of_def = function
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
      
let rec is_ty_equal t1 t2 = match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool -> true
  | Fun(args1, ret1), Fun(args2, ret2) -> (try List.for_all2 is_ty_equal args1 args2 with Invalid_argument _ -> false) && is_ty_equal ret1 ret2
  | Struct(xts), Struct(yts) -> (try List.for_all2 (fun (_, t1) (_, t2) -> is_ty_equal t1 t2) xts yts with Invalid_argument _ -> false)
  | Box, Box -> true
  | Pointer(t1), Pointer(t2) -> is_ty_equal t1 t2
  | NameTy(_, x), _ -> is_ty_equal x t2
  | _, NameTy(_, y) -> is_ty_equal t1 y
  | _ -> false
      
let assign (e1, t) e2 =
  if !enable_gc then
    Assign(e1, e2)
  else
    match t, e2 with
      | Box, CallDir _ -> Assign(e1, e2)
      | Box, _ -> Seq(Assign(e1, e2), Exp(CallDir(Var("add_ref"), [e1])))
      | _ -> Assign(e1, e2)

let rec insert_return t = 
  let rec return = function
    | Var(x) as e -> 
	if t = Box && not !enable_gc then 
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
  let rec collect_decs = function
    | Dec(xt, None) -> [VarDec(xt, None)], Exp(Nop)
    | Dec(xt, Some(e)) -> [VarDec(xt, None)], (assign (Var(fst xt), snd xt) e)
    | Seq(s1, s2) -> 
	let decs1, s1' = collect_decs s1 in
	let decs2, s2' = collect_decs s2 in
	  decs2 @ decs1, Seq(s1', s2')
    | s -> [], s in
  let rec remove_nop = function
    | Seq(Exp(Nop), s') -> (remove_nop s')
    | Seq(s', Exp(Nop)) -> (remove_nop s')
    | Seq(s1', s2') -> Seq(remove_nop s1', remove_nop s2')
    | s -> s in
  let release_boxes decs s = 
    if !enable_gc then
      s
    else
      let rec insert_release x s =
	let e = Exp(CallDir(Var("release"), [Var(x)])) in
	  match s with
	    | Return _ -> Seq(e, s)
	    | Seq(s1, s2) -> Seq(s1, insert_release x s2)
	    | s -> Seq(s, e) in
	List.fold_left 
	  (fun s dec -> 
	    match dec with 
	      | VarDec((x, Box), _) -> insert_release x s
	      | _ -> s)
	  s decs in
  let decs, s' = collect_decs s in
    Block(List.rev decs, release_boxes decs (remove_nop s'))

let rec wrapper (x, t) =
  match t with
    | Fun(args, r) ->
	assert(List.length args = 1);
	let t = List.hd args in
	let y = gentmp t in
	  FunDef({ name = Id.L(x);
		   args = [(y, t)];
		   body = block (insert_return Box (Seq(Dec(("p", Box), None), 
							Seq(Assign(Var("p"), CallDir(Var("new_sp"), [Sizeof(t)])),
							    Seq(Assign(Deref(Cast(Pointer(t), CallDir(Var("sp_get"), [Var("p")]))), Var(y)),
								Exp(Var("p")))))));
		   ret = r },
		ref false)
    | NameTy(_, t) -> wrapper (x, t)
    | t -> D.printf "invalid type : %s\n" (string_of_type t); assert false

let rec unwrapper (x, t) =
  match t with
    | Fun(args, r) ->
	assert(List.length args = 1);
	let t = List.hd args in
	let y = gentmp t in
	  FunDef({ name = Id.L(x);
		   args = [(y, t)];
		   body = block (insert_return r (Exp(Deref(Cast(Pointer(r), CallDir(Var("sp_get"), [Var(y)]))))));
		 ret = r }, 
		ref false)
    | NameTy(_, t) -> unwrapper (x, t)
    | t -> failwith ("invalid type : " ^ (string_of_type t))

let rec concat e1 xt e2 = match e1 with
  | Exp(exp) -> 
      (match xt with
	| x, Void -> Seq(e1, e2)
	| xt -> Seq(Dec(xt, Some(exp)), e2))
  | Seq(e1', e2') -> Seq(e1', concat e2' xt e2)
  | _ -> raise (Concat(e1))

let toplevel : def list ref = ref []

let maker_name t = "_make" ^ (string_of_type t)
let make_closure t = 
  let name = maker_name t in
  let args, body = 
    match t with
      | NameTy(_, Struct(xts)) -> 
	  let c = gentmp t in
	  let s' = List.fold_right (fun (x, t) s -> (Seq(Assign(Field(c, x), Var(x)), s))) xts (Exp(Var(c))) in
	    xts, Seq(Dec((c, t), None), s')
      | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
    name, FunDef({ name = Id.L(name); args = args; body = block (insert_return t body); ret = t }, ref false)

let applyer_name t = "_apply" ^ (string_of_type t)
let rec apply_closure t args =  
  let x = gentmp t in
  let name = applyer_name t in
  let args = (x, t) :: (List.map (fun t -> gentmp t, t) args) in
  let (f, t'), zts = match t with NameTy(_, Struct(xt::xts)) -> (xt, xts) | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
  let ret = match t' with Fun(_, ret) -> ret | _ -> assert false in
  let body = Exp(CallDir(Field(x, f), (List.map (fun (y, _) -> Var(y)) (List.tl args)) @ (List.map (fun (z, _) -> Field(x, z)) zts))) in
    ret, FunDef({ name = Id.L(name); args = args; body = block (insert_return ret body); ret = ret }, ref false)

let find_typedef t = 
  List.find (fun def -> match def with | TypeDef((x, t'), _ )-> is_ty_equal t t' | _ -> false) !toplevel

let find_namety t = 
  match find_typedef t with
    | TypeDef((name, t), _) -> NameTy(name, t)
    | _ -> assert false
	
let namety t =
  try
    find_namety t
  with
      Not_found ->
	let name = (gentmp t) ^ "_t" in
	  toplevel := TypeDef((name, t), ref false) :: !toplevel;
	  NameTy(name, t)
	    
let rec return_ty = function
  | Fun(_, ret) -> ret
  | NameTy(_, t) -> return_ty t
  | _ -> assert false
	  
(* 型変換 *)
let rec k t = 
  let _ = D.printf "C.k %s\n" (Type.string_of_typ t) in
  match t with
  | Type.Var _ -> Box
  | Type.App(Type.Unit, []) -> Void
  | Type.App(Type.Bool, []) -> Bool
  | Type.App(Type.Int, []) -> Int
  | Type.App(Type.Arrow, xs) -> namety (Fun(List.map k (L.init xs), k (L.last xs))) (* Fun型には名前をつける *)
  | Type.App(Type.TyFun(_, _), _) -> assert false (* not implemented. is possible ? *)
  | Type.Poly([], t) -> k t
  | Type.Poly _ -> assert false (* not implemented *)
  | Type.Meta _ -> assert false (* not implemented *)
  | _ -> assert false

let find_closure_ty yts t = find_namety (Struct(("f", t) :: yts))
  
let rec closure_ty yts t = match t with
  | Fun(args, ret) -> 
      let name = (Id.genid "closure") ^ "_t" in name, Struct(("f", t) :: yts)
  | NameTy(_, t') -> closure_ty yts t'
  | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false

let rec split = function
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
    | e, Box when not !enable_gc ->
	let x = gentmp t in
	let e2, t2 = k (Var(x)) in
	  Let((x, t), e, e2), t2
    | e, _ -> k e

let rec insert_dec e k = 
  match e with
    | Let(xt, e1, e2) -> Seq(Dec(xt, Some(e1)), insert_dec e2 k)
    | e -> k e
      
let rec g' env ids e = 
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
      | Closure.Bool(b) -> BoolExp(b), Bool
      | Closure.Int(i) -> IntExp(i), Int
      | Closure.Not(e) -> unop e (fun e -> Not(e)) Bool
      | Closure.Neg(e) -> unop e (fun e -> Neg(e)) Int
      | Closure.Add(e1, e2) -> binop e1 e2 (fun e1 e2 -> Add(e1, e2)) Int
      | Closure.Sub(e1, e2) -> binop e1 e2 (fun e1 e2 -> Sub(e1, e2)) Int
      | Closure.Mul(e1, e2) -> binop e1 e2 (fun e1 e2 -> Mul(e1, e2)) Int
      | Closure.Div(e1, e2) -> binop e1 e2 (fun e1 e2 -> Div(e1, e2)) Int
      | Closure.Eq(e1, e2) -> binop e1 e2 (fun e1 e2 -> Eq(e1, e2)) Bool
      | Closure.LE(e1, e2) -> binop e1 e2 (fun e1 e2 -> LE(e1, e2)) Bool
      | Closure.Var(x) -> 
	  let x = rename ids x in 
	    Var(x), (M.find x env)
      | Closure.App(x, ys) -> 
	  let rec apply (x, t) = match t with
	    | NameTy(_, Struct(_)) -> 
		let rec bind xts = function 
		  | [] ->
		      let rt, fundef = apply_closure t (List.map snd xts) in
			toplevel := fundef :: !toplevel;
			CallDir(Var(applyer_name t), x :: (List.map fst xts)), rt
		  | e2 :: e2s ->
		      let e2', t' = g' env ids e2 in
			insert_let (e2', t')
			  (fun x -> bind (xts @ [(x, t')]) e2s) in
		  bind [] ys (* left-to-right evaluation *)
	    | NameTy(_, t) -> apply (x, t)
	    | Fun(args, rt) ->
		let rec bind xts = function 
		  | [] ->
		      assert(List.for_all2 is_ty_equal (List.map snd xts) args);
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
	  let rec bind xts = function 
	    | [] ->
		CallDir(Var(l), List.map fst xts), (return_ty (M.find l env))
	    | e2 :: e2s ->
		let e2', t' = g' env ids e2 in
		  insert_let (e2', t')
		    (fun x -> bind (xts @ [(x, t')]) e2s) in
	    bind [] ys (* left-to-right evaluation *) 

let rec g env ids e = (* C言語コード生成 (caml2html: c_g) *)
  let _ = D.printf "C.g %s\n" (Closure.string_of_exp e) in
    match e with 
  | Closure.Unit -> Exp(Nop), Void
  | Closure.Nil(t) -> let t = k t in Exp(Nil(t)), t
  | Closure.Exp(e) -> 
      let e', t = g' env ids e in 
	(insert_dec e' (fun e -> Exp(e))), t
  | Closure.Cons(x, y) -> assert false (* not implemented *)
  | Closure.If(e, e1, e2) -> 
      let e, _ = g' env ids e in
      let (s1, t1) = (g env ids e1) in
      let (s2, t2) = (g env ids e2) in
	insert_dec e (fun e ->
	  assert (is_ty_equal t1 t2);
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
		  let t = NameTy(name, t) in
		  let maker, fundef = make_closure t in
		    toplevel := fundef :: !toplevel;
		    t, maker) in
      let x' = gentmp t in (* クロージャはトップレベル関数と名前が被っているので改名する *)
      let e2', t2 = g (M.add x' t env) (M.add x x' ids) e2 in
	Seq(Dec((x', t), Some(MakeClosure(Id.L(maker), l, yts))), e2'), t2

(* トップレベル関数の C 言語変換 (caml2html: c_h) *)
let h env { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let () = D.printf "C.h (Id.L(%s), %s)\n" x (Type.string_of_typ t) in
  let yts = List.map (fun (y, t) -> (y, k t)) yts in
  let zts = List.map (fun (z, t) -> (z, k t)) zts in
  let body, t' = g (M.add x (k t) (M.add_list yts (M.add_list zts env))) M.empty e in 
    match t with
      | Type.App(Type.Arrow, _) ->
	  (* 戻り値がクロージャになっているケースがあるので、シグネチャの型ではなく変換後の方を使用する。ただし型環境には変換前の方を入れているので再帰関数については再考が必要 *)
	  let body' = block (if (is_ty_equal t' Void) then body else (insert_return t' body)) in
	    toplevel := FunDef({ name = Id.L(x); args = yts @ zts; body = body'; ret = t' }, ref false) :: !toplevel; 
	  M.add x (Fun(List.map snd (yts @ zts), t')) env
      | _ -> Printf.eprintf "invalid type : %s\n" (Type.string_of_typ t); assert false
      
(* プログラム全体のCコード生成 (caml2html: c_f) *)
let f (Closure.Prog(defs, e)) =
  (* 型変換の k で toplevel に TypeDef を追加する可能性があるので、必ず k の結果を let で束縛してから toplevel を評価すること *)
  List.iter (fun (x, t) -> let w = wrapper (x, k t) in toplevel := w :: !toplevel) (Wrap.wrapper_list ());
  List.iter (fun (x, t) -> let u = unwrapper (x, k t) in toplevel := u :: !toplevel) (Wrap.unwrapper_list ());
  let env = (M.map k !Typing.extenv) in
  let env' = List.fold_left h env defs in
  let e, _ = g env' M.empty e in
  let main = FunDef({ name = Id.L("main");
		      args = [];
		      body = block (insert_return Int 
				       (if !enable_gc then
					 (Seq(Exp(CallDir(Var("GC_init"), [])), (Seq(e, Exp(IntExp(0))))))
					 else
					   (Seq(e, Exp(IntExp(0))))));
		      ret = Int }, 
		   ref true) in
    (Prog(List.rev !toplevel @ [main]))
