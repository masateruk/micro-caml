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
    | IfEq of Id.t * Id.t * t * t
    | IfLE of Id.t * Id.t * t * t
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
    | Not of Id.t
    | Neg of Id.t
    | Add of Id.t * Id.t
    | Sub of Id.t * Id.t
    | Mul of Id.t * Id.t
    | Div of Id.t * Id.t
    | Cons of Id.t * Id.t
    | Var of Id.t
    | CondEq of Id.t * Id.t * exp * exp
    | CondLE of Id.t * Id.t * exp * exp
    | CallCls of (Id.t * ty) * Id.t list
    | CallDir of exp * exp list
    | MakeClosure of Id.l * Id.t * (Id.t * ty) list
    | Field of Id.t * Id.t
    | Sizeof of ty
    | Ref of exp
    | Deref of exp
    | Cast of ty * exp
type fundef = { name : Id.l; args : (Id.t * ty) list; body : t; ret : ty }
type def =
    | FunDef of fundef
    | TypeDef of (Id.t * ty)
type prog = Prog of def list * t

exception Concat of t

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

let rec is_ty_equal t1 t2 = match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool -> true
  | Fun(args1, ret1), Fun(args2, ret2) -> (try List.for_all2 is_ty_equal args1 args2 with Invalid_argument _ -> false) && is_ty_equal ret1 ret2
  | Struct(xts), Struct(yts) -> (try List.for_all2 (fun (x, t1) (y, t2) -> x = y && is_ty_equal t1 t2) xts yts with Invalid_argument _ -> false)
  | NameTy(_, x), _ -> is_ty_equal x t2
  | _, NameTy(_, y) -> is_ty_equal t1 y
  | Box, Box -> true
  | _ -> false

let rec insert_return = function
  | Seq(s1, s2) -> Seq(s1, insert_return s2)
  | Assign(e1, e2) as s-> Seq(s, Return e1)
  | Exp(e) -> Return(e)
  | IfEq(x, y, s1, s2) -> IfEq(x, y, insert_return s1, insert_return s2)
  | IfLE(x, y, s1, s2) -> IfLE(x, y, insert_return s1, insert_return s2)
  | Block(decs, s') -> Block(decs, insert_return s')
  | _ -> assert false

let block s =
  let rec collect_decs = function
    | Dec(xt, None) -> [VarDec(xt, None)], Exp(Nop)
    | Dec(xt, Some(e)) -> [VarDec(xt, None)], Assign(Var(fst xt), e)
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
  let decs, s' = collect_decs s in
    Block(List.rev decs, remove_nop s')

let string_of_indent depth = String.make (depth * 2) ' '

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
      | Box -> (string_of_indent depth) ^ "box_t" ^ (plus x)
      | Pointer t -> (string_of_indent depth) ^ (string_of_type t) ^ "*" ^ (plus x)

let rec string_of_exp = function
  | Nop -> ""
  | Nil _ -> "nil"
  | BoolExp(b) -> string_of_bool b
  | IntExp(i) -> string_of_int i
  | Not(x) -> "!" ^ x
  | Neg(x) -> "-" ^ x
  | Add(x, y) -> x ^ " + " ^ y
  | Sub(x, y) -> x ^ " - " ^ y
  | Mul(x, y) -> x ^ " * " ^ y
  | Div(x, y) -> x ^ " / " ^ y
  | Cons(x, y) -> x ^ " :: " ^ y
  | Var(x) -> x
  | CondEq(x, y, e1, e2) -> x ^ " == " ^ y ^ " ? " ^ (string_of_exp e1) ^ " : " ^ (string_of_exp e2)
  | CondLE(x, y, e1, e2) -> x ^ " < " ^ y ^ " ? " ^ (string_of_exp e1) ^ " : " ^ (string_of_exp e2)
  | CallCls((x, t), xs) -> "apply_" ^ (string_of_type t) ^ "(" ^ x ^ ", " ^ (String.concat ", " xs) ^ ")"
  | CallDir(x, xs) ->  (string_of_exp x) ^ "(" ^ (String.concat ", " (List.map string_of_exp xs)) ^ ")"
  | MakeClosure(Id.L(l), x, yts) -> l ^ "(" ^ x ^ ", " ^ (String.concat ", " (List.map fst yts)) ^ ")" 
  | Field(x, y) -> x ^ "." ^ y
  | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
  | Ref(e) -> "&" ^ (string_of_exp e)
  | Deref(e) -> "*(" ^ (string_of_exp e) ^ ")"
  | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_exp e)

let rec string_of_separator = function
  | IfEq _ -> ""
  | IfLE _ -> ""
  | Seq(_, s) -> string_of_separator s
  | Block _ -> ""
  | _ -> ";"

let rec string_of_prog (Prog(defs, s)) =
  let rec string_of_statement depth = function
    | Dec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | Dec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e)
    | Assign(x, e) -> (string_of_indent depth) ^ (string_of_exp x) ^ " = " ^ (string_of_exp e)
    | Exp(e) -> (string_of_indent depth) ^ (string_of_exp e) 
    | IfEq(x, y, s1, s2) -> (string_of_indent depth) ^ "if (" ^ x ^ " == " ^ y ^ ") " ^ (string_of_statement depth s1) ^ " else " ^ (string_of_statement depth s2)
    | IfLE(x, y, s1, s2) -> (string_of_indent depth) ^ "if (" ^ x ^ " < " ^ y ^ ") " ^ (string_of_statement depth s1) ^ " else " ^ (string_of_statement depth s2)
    | Return(e) -> (string_of_indent depth) ^ "return " ^ (string_of_exp e)
    | Seq(s1, s2) -> (string_of_statement depth s1) ^ (string_of_separator s1) ^ "\n" ^ (string_of_statement depth s2) 
    | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
    | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (string_of_indent depth) ^ "}" 
  and string_of_dec depth = function
    | VarDec((x, t), None) -> (string_of_indent depth) ^ (string_of_type ~x:x t)
    | VarDec((x, t), Some(e)) -> (string_of_indent depth) ^ (string_of_type ~x:x t) ^ " = " ^ (string_of_exp e) in
  let string_of_def = function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }) ->
	let t' = string_of_type t in
	let name' = x in
	let args' = String.concat ", " (List.map (fun (y, t) -> (string_of_type ~x:y t)) yts) in
	let body' = string_of_statement 0 s in
	  t' ^ " " ^ name' ^ "(" ^ args' ^ ")\n" ^ body' ^ "\n"
    | TypeDef(x, t) ->
	"typedef " ^ (string_of_type ~x:x t) ^ ";\n" in
      "#include <ucaml.h>\n" ^ 
      "\n" ^ (String.concat "\n" (List.map string_of_def defs)) ^ "\n" ^
      "int main()\n" ^
      (string_of_statement 0 (insert_return (block (Seq(Exp(CallDir(Var("GC_init"), [])), (Seq(s, Exp(IntExp(0))))))))) ^ "\n"

let wrapper (x, t) =
  match t with
    | Fun(args, r) ->
	assert(List.length args = 1);
	let t = List.hd args in
	let y = gentmp t in
	  FunDef({ name = Id.L(x);
		   args = [(y, t)];
		   body = block (insert_return (Seq(Dec(("b", Box), None), 
						    Seq(Assign(Field("b", "p"), CallDir(Var("GC_malloc"), [Sizeof(t)])),
							Seq(Assign(Deref(Cast(Pointer(t), Field("b", "p"))), Var(y)),
							    Exp(Var("b")))))));
		 ret = r })
    | t -> failwith ("invalid type : " ^ (string_of_type t))

let unwrapper (x, t) =
  match t with
    | Fun(args, r) ->
	assert(List.length args = 1);
	let t = List.hd args in
	let y = gentmp t in
	  FunDef({ name = Id.L(x);
		   args = [(y, t)];
		   body = block (insert_return (Exp(Deref(Cast(Pointer(r), Field(y, "p"))))));
		 ret = r })
    | t -> failwith ("invalid type : " ^ (string_of_type t))

let rec concat e1 xt e2 = match e1 with
  | Exp(exp) -> 
      (match xt with
	| x, Void -> Seq(e1, e2)
	| xt -> Seq(Dec(xt, Some(exp)), e2))
  | Seq(e1', e2') -> Seq(e1', concat e2' xt e2)
  | _ -> raise (Concat(e1))

let toplevel : def list ref = ref []

let maker_name t = "make_" ^ (string_of_type t)
let make_closure t = 
  let name = maker_name t in
  let args, body = 
    match t with
      | NameTy(_, Struct(xts)) -> 
	  let c = gentmp t in
	  let s' = List.fold_right (fun (x, t) s -> (Seq(Assign(Field(c, x), Var(x)), s))) xts (Exp(Var(c))) in
	    xts, Seq(Dec((c, t), None), s')
      | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
    name, FunDef({ name = Id.L(name); args = args; body = block (insert_return body); ret = t })

let rec apply_closure t args =  
  let x = gentmp t in
  let name = "apply_" ^ (string_of_type t) in
  let args = (x, t) :: (List.map (fun t -> gentmp t, t) args) in
  let (f, t'), zts = match t with NameTy(_, Struct(xt::xts)) -> (xt, xts) | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false in
  let ret = match t' with Fun(_, ret) -> ret | _ -> assert false in
  let body = Exp(CallDir(Field(x, f), (List.map (fun (y, _) -> Var(y)) (List.tl args)) @ (List.map (fun (z, _) -> Field(x, z)) zts))) in
    ret, FunDef({ name = Id.L(name); args = args; body = block (insert_return body); ret = ret })

(* 型変換 *)
let rec k t = 
  let _ = D.printf "C.k %s\n" (Type.string_of_typ t) in
  match t with
  | Type.Var _ -> Box
  | Type.App(Type.Unit, []) -> Void
  | Type.App(Type.Bool, []) -> Bool
  | Type.App(Type.Int, []) -> Int
  | Type.App(Type.Arrow, xs) -> Fun(List.map k (L.init xs), k (L.last xs))
  | Type.App(Type.TyFun(_, _), _) -> assert false (* not implemented. is possible ? *)
  | Type.Poly([], t) -> k t
  | Type.Poly _ -> assert false (* not implemented *)
  | Type.Meta _ -> assert false (* not implemented *)
  | _ -> assert false

let find_closure_def yts t = 
    let typedef = List.find (fun def -> match def with TypeDef(_, Struct(("f", t') :: yts')) -> (is_ty_equal t t') && (try List.for_all2 (fun (y, t) (y', t') -> (is_ty_equal t t')) yts yts' with Invalid_argument _ -> false)  | _ -> false) !toplevel in
      match typedef with
	| TypeDef(name, t) -> NameTy(name, t)
	| _ -> assert false

let rec closure_ty yts t = match t with
  | Fun(args, ret) -> 
      let name = (Id.genid "closure") ^ "_t" in name, Struct(("f", t) :: yts)
  | NameTy(_, t') -> closure_ty yts t'
  | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false

let rec split = function
  | Dec _ -> assert false
  | Assign(e, _) as s -> Some(s), e
  | Exp(e) -> None, e
  | IfEq(_, _, s1, s2) as s -> (match s1, s2 with Assign(e1, _), Assign(e2, _) when e1 = e2 -> Some(s), e1 | _ -> assert false)
  | IfLE(_, _, s1, s2) as s -> (match s1, s2 with Assign(e1, _), Assign(e2, _) when e1 = e2 -> Some(s), e1 | _ -> assert false)
  | Return _ -> assert false
  | Seq(s1, s2) -> 
      let s2', e2' = split s2 in
	(match s2' with
	  | Some(s2') -> Some(Seq(s1, s2')), e2'
	  | None -> Some(s1), e2')
  | Block(_, s) -> split s

(* 改名。改名マップに ID.t があれば改名後の名前を返す。なければ、元の名前を返す。*)      
let rename m x = if M.mem x m then M.find x m else x
   
let rec g env ids e = (* C言語コード生成 (caml2html: virtual_g) *)
  let _ = D.printf "C.g %s\n" (Closure.string_of_exp e) in
  match e with 
  | Closure.Unit -> Exp(Nop), Void
  | Closure.Nil(t) -> let t = k t in Exp(Nil(t)), t
  | Closure.Bool(b) -> Exp(BoolExp(b)), Bool
  | Closure.Int(i) -> Exp(IntExp(i)), Int
  | Closure.Not(x) -> Exp(Not(rename ids x)), Bool
  | Closure.Neg(x) -> Exp(Neg(rename ids x)), Int
  | Closure.Add(x, y) -> Exp(Add(rename ids x, rename ids y)), Int
  | Closure.Sub(x, y) -> Exp(Sub(rename ids x, rename ids y)), Int
  | Closure.Mul(x, y) -> Exp(Mul(rename ids x, rename ids y)), Int
  | Closure.Div(x, y) -> Exp(Div(rename ids x, rename ids y)), Int
  | Closure.Cons(x, y) -> assert false (* not implemented *)
  | Closure.IfEq(x, y, e1, e2) -> 
      let x = rename ids x in
      let y = rename ids y in
      let (s1, t1) = (g env ids e1) in
      let (s2, t2) = (g env ids e2) in
	assert (is_ty_equal t1 t2);
	(match s1, s2 with
	  | Exp(e1'), Exp(e2') -> Exp(CondEq(x, y, e1', e2')), t1
	  | _, _ -> 
	      let s1', e1' = split s1 in
	      let s2', e2' = split s2 in
		(match s1', s2' with 
		  | Some(s1'), Some(s2') -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | Some(s1'), None -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Assign(Var(z), e2'))), Exp(Var(z))))
		  | None, Some(s2') -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Assign(Var(z), e1')), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | None, None -> 
		      Exp(CondEq(x, y, e1', e2'))), t1)
  | Closure.IfLE(x, y, e1, e2) -> 
      let x = rename ids x in
      let y = rename ids y in
      let s1, t1 = (g env ids e1) in
      let s2, t2 = (g env ids e2) in
	assert (is_ty_equal t1 t2);
	(match s1, s2 with
	  | Exp(e1'), Exp(e2') -> Exp(CondLE(x, y, e1', e2')), t1
	  | _, _ -> 
	      let s1', e1' = split s1 in
	      let s2', e2' = split s2 in
		(match s1', s2' with 
		  | Some(s1'), Some(s2') -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | Some(s1'), None -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Assign(Var(z), e2'))), Exp(Var(z))))
		  | None, Some(s2') -> 
		      let z = gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Assign(Var(z), e1')), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | None, None -> Exp(CondLE(x, y, e1', e2'))), t1)
  | Closure.Let((x, t), e1, e2) -> 
      let x = rename ids x in
      let e1', t1 = g env ids e1 in
      let e2', t2 = g (M.add x t1 env) ids e2 in
	(concat e1' (x, t1) e2'), t2
  | Closure.Var(x) -> 
      let x = rename ids x in 
	Exp(Var(x)), (M.find x env)
  | Closure.MakeCls((x, _), { Closure.entry = Id.L(l); Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: c_makecls) *)
      let ys = List.map (rename ids) ys in
      let yts = List.map (fun y -> (y, M.find y env)) ys in
      let t, maker = 
	(try
	    let t = find_closure_def yts (M.find l env) in
	    let maker = maker_name t in
	      t, maker
	  with
	      Not_found ->
		let name, t = closure_ty yts (M.find l env) in (* (x, t) の型 t は Type.Arrow（関数）型なので型環境からトップレベルの型を引いてクロージャ型に変換する *)
		  toplevel := TypeDef(name, t) :: !toplevel;
		  let t = NameTy(name, t) in
		  let maker, fundef = make_closure t in
		    toplevel := fundef :: !toplevel;
		    t, maker) in
      let x' = gentmp t in (* クロージャはトップレベル関数と名前が被っているので改名する *)
      let e2', t2 = g (M.add x' t env) (M.add x x' ids) e2 in
	Seq(Dec((x', t), Some(MakeClosure(Id.L(maker), l, yts))), e2'), t2
  | Closure.AppCls(x, ys) -> 
      let x = rename ids x in
      let ys = List.map (rename ids) ys in
      let t = M.find x env in
	begin
	  match t with 
	    | NameTy(_, Struct(_)) -> 
		let ret, fundef = apply_closure t (List.map (fun y -> (M.find y env)) ys) in
		  toplevel := fundef :: !toplevel;
		  Exp(CallCls((x, t), ys)), ret
	    | Fun(args, rt) ->
		assert(List.for_all2 is_ty_equal (List.map (fun y -> M.find y env) ys) args);
		Exp(CallDir(Var(x), List.map (fun y -> Var(y)) ys)), rt
	    | _ -> Printf.eprintf "invalid type : %s\n" (string_of_type t); assert false
	end
  | Closure.AppDir(Id.L(l), ys) -> 
      let ys = List.map (rename ids) ys in
      let t = match M.find l env with Fun(_, ret) -> ret | _ -> assert false in
	Exp(CallDir(Var(l), List.map (fun y -> Var(y)) ys)), t

(* トップレベル関数の C 言語変換 (caml2html: c_h) *)
let h env { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let yts = List.map (fun (y, t) -> (y, k t)) yts in
  let zts = List.map (fun (z, t) -> (z, k t)) zts in
  let body, t' = g (M.add x (k t) (M.add_list yts (M.add_list zts env))) M.empty e in 
    match t with
      | Type.App(Type.Arrow, _) ->
	  (* 戻り値がクロージャになっているケースがあるので、シグネチャの型ではなく変換後の方を使用する。ただし型環境には変換前の方を入れているので再帰関数については再考が必要 *)
	  toplevel := FunDef({ name = Id.L(x); args = yts @ zts; body = block (insert_return body); ret = t' }) :: !toplevel; 
	  M.add x (Fun(List.map snd (yts @ zts), t')) env
      | _ -> assert false
      
(* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) *)
let f (Closure.Prog(defs, e)) =
  List.iter (fun (x, t) -> toplevel := wrapper (x, k t) :: !toplevel) (Wrap.wrapper_list ());
  List.iter (fun (x, t) -> toplevel := unwrapper (x, k t) :: !toplevel) (Wrap.unwrapper_list ());
  let env = (M.map k !Typing.extenv) in
  let env' = List.fold_left h env defs in
  let e, _ = g env' M.empty e in
    (string_of_prog (Prog(List.rev !toplevel, e)))
      
