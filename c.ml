type t = (* C言語の文の列 *)
    | Dec of (Id.t * Type.t) * exp option
    | Assign of exp * exp
    | Exp of exp
    | IfEq of Id.t * Id.t * t * t
    | IfLE of Id.t * Id.t * t * t
    | Return of exp
    | Seq of t * t
    | Block of dec list * t
and dec =
    | VarDec of (Id.t * Type.t) * exp option
and exp = (* C言語の文の列 *) 
    | Nop
    | Bool of bool
    | Int of int
    | Not of Id.t
    | Neg of Id.t
    | Add of Id.t * Id.t
    | Sub of Id.t * Id.t
    | Mul of Id.t * Id.t
    | Div of Id.t * Id.t
    | Var of Id.t
    | Fun of Id.t
    | CondEq of Id.t * Id.t * exp * exp
    | CondLE of Id.t * Id.t * exp * exp
    | CallCls of (Id.t * Type.t) * Id.t list
    | CallDir of exp * exp list
    | MakeClosure of Id.l * Id.l * (Id.t * Type.t) list
    | Field of Id.t * Id.t
type fundef = { name : Id.l; args : (Id.t * Type.t) list; body : t; ret : Type.t }
type def =
    | FunDef of fundef
    | TypeDef of (Id.t * Type.t)
type prog = Prog of def list * t

exception Concat of t

let predefined = ref M.empty

let rec iota n first = 
  if n = 0 then [] else first :: iota (n - 1) (first + 1)

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
      | Type.Unit -> (string_of_indent depth) ^ "void" 
      | Type.Bool -> (string_of_indent depth) ^ "bool" ^ (plus x)
      | Type.Int -> (string_of_indent depth) ^ "int" ^ (plus x)
      | Type.Var({ contents = Some(t)}) -> (string_of_indent depth) ^ (string_of_type t) ^ (plus x)
      | Type.Fun(args, ret, ys) -> (string_of_indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map string_of_type (args @ ys))) ^ ")"
      | Type.Closure(f, ys) -> 
	  (string_of_indent depth) ^ "struct {\n" ^ (string_of_type ~depth:(depth + 1) f) ^ " f; \n" ^ 
	    (String.concat "\n" 
		(List.map (fun (y, i) -> (string_of_type ~depth:(depth + 1) y) ^ " fv" ^ (string_of_int i) ^ ";") (List.combine ys (iota (List.length ys) 1))))
	      ^ "\n" ^ (string_of_indent depth) ^ "}" ^ (plus x)
      | Type.Record(xts) -> 
	  (string_of_indent depth) ^ "struct {\n" ^ (String.concat ";\n" (List.map (fun (x, t) -> string_of_type ~x:x ~depth:(depth + 1) t) xts)) ^ ";\n" ^ (string_of_indent depth) ^ "}" ^ (plus x)
      | Type.Name(x', _) -> (string_of_indent depth) ^ x' ^ (plus x)
      | _ -> assert false 

let rec string_of_exp = function
  | Nop -> ""
  | Bool(b) -> string_of_bool b
  | Int(i) -> string_of_int i
  | Not(x) -> "!" ^ x
  | Neg(x) -> "-" ^ x
  | Add(x, y) -> x ^ " + " ^ y
  | Sub(x, y) -> x ^ " - " ^ y
  | Mul(x, y) -> x ^ " * " ^ y
  | Div(x, y) -> x ^ " / " ^ y
  | Var(x) -> x
  | Fun(x) -> x
  | CondEq(x, y, e1, e2) -> x ^ " == " ^ y ^ " ? " ^ (string_of_exp e1) ^ " : " ^ (string_of_exp e2)
  | CondLE(x, y, e1, e2) -> x ^ " < " ^ y ^ " ? " ^ (string_of_exp e1) ^ " : " ^ (string_of_exp e2)
  | CallCls((x, t), xs) -> "apply_" ^ (string_of_type t) ^ "(" ^ x ^ ", " ^ (String.concat ", " xs) ^ ")"
  | CallDir(Fun(x), xs) when M.mem x !predefined -> let (_, f) = M.find x !predefined in f x xs
  | CallDir(x, xs) ->  (string_of_exp x) ^ "(" ^ (String.concat ", " (List.map string_of_exp xs)) ^ ")"
  | MakeClosure(Id.L(mk), Id.L(x), yts) -> mk ^ "(" ^ x ^ ", " ^ (String.concat ", " (List.map fst yts)) ^ ")" 
  | Field(x, y) -> x ^ "." ^ y

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
    "#include <stdio.h>\n" ^ 
      "#include <stdbool.h>\n\n" ^ 
      (String.concat "\n" (List.map string_of_def defs)) ^ "\n" ^
      "int main()\n" ^
      (string_of_statement 0 (insert_return (block (Seq(s, Exp(Int(0))))))) ^ "\n"

let rec concat e1 xt e2 =
  match e1 with
  | Exp(exp) -> Seq(Dec(xt, Some(exp)), e2)
  | Seq(e1', e2') -> Seq(e1', concat e2' xt e2)
  | _ -> raise (Concat(e1))

let toplevel : def list ref = ref []

let toplevel_fun name free_variables = match free_variables with
  | [] -> name
  | _ -> "_" ^ name (* 自由変数がある場合は元の名前をクロージャとして使用するため自由変数を補完した関数は別名をつける *)

let rec make_closure (x, t) l yts e =
  let name' = "make_" ^ (string_of_type t) in
  let args', body' = 
    match t with
      | Type.Name(_, Type.Record(xts)) -> 
	  let c = Id.gentmp t in
	  let s' : t = List.fold_right (fun (x, t) s -> (Seq(Assign(Field(c, x), Var(x)), s))) xts (Exp(Var(c))) in
	    xts, Seq(Dec((c, t), None), s')
      | _ -> Printf.eprintf "%s\n" (string_of_type t); assert false in
    (if not (List.exists (function FunDef({ name = Id.L(name); args = _; body = _; ret = _ }) -> name = name' | _ -> false) !toplevel) then
      toplevel := FunDef({ name = Id.L(name'); args = args'; body = block (insert_return body'); ret = t }) :: !toplevel
      else
	());
    Seq(Dec((x, t), Some(MakeClosure(Id.L(name'), l, yts))), e)

let rec apply_closure t args =  
  let x' = Id.gentmp t in
  let name' = "apply_" ^ (string_of_type t) in
  let args' = (x', t) :: (List.map (fun t -> Id.gentmp t, t) args) in
  let (f, t'), zts = match t with Type.Name(_, Type.Record(xt::xts)) -> (xt, xts) | _ -> assert false in
  let ret' = match t' with Type.Fun(_, ret, _) -> ret | _ -> assert false in
  let body' = Exp(CallDir(Field(x', f), (List.map (fun (y, _) -> Var(y)) (List.tl args')) @ (List.map (fun (z, _) -> Field(x', z)) zts))) in
    if not (List.exists (function FunDef({ name = Id.L(name); args = _; body = _; ret = _ }) -> name = name' | _ -> false) !toplevel) then
      toplevel := FunDef({ name = Id.L(name'); args = args'; body = block (insert_return body'); ret = ret' }) :: !toplevel
    else
      ()

let rec k t = match t with
  | Type.Fun(args, ret, fv) -> 
      Type.Fun(List.map k args, k ret, List.map k fv)
  | Type.Closure(f, fv) -> 
      let f = k f in
      let rt = Type.Record(("f", f) :: (List.map (fun (i, t) -> ("fv" ^ (string_of_int i), t)) (List.combine (iota (List.length fv) 1) fv))) in
      let x = 
	try 
	  let (x', _) = List.find (fun (_, t) -> Type.eq rt t) (List.concat (List.map (function TypeDef(xt) -> [xt] | _ -> []) !toplevel)) in 
	    x'
	with
	    Not_found -> 
	      let x = Id.gentype t in
		toplevel := TypeDef(x, rt) :: !toplevel; x in
	Type.Name(x, rt)
  | Type.Record(xts) -> Type.Record(List.map (fun (x, t) -> (x, k t)) xts)
  | t -> t

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
      
let rec g env = function (* 式の仮想マシンコード生成 (caml2html: virtual_g) *)
  | Closure.Unit -> Exp(Nop), Type.Unit
  | Closure.Bool(b) -> Exp(Bool(b)), Type.Bool
  | Closure.Int(i) -> Exp(Int(i)), Type.Int
  | Closure.Not(x) -> Exp(Not(x)), Type.Bool
  | Closure.Neg(x) -> Exp(Neg(x)), Type.Int
  | Closure.Add(x, y) -> Exp(Add(x, y)), Type.Int
  | Closure.Sub(x, y) -> Exp(Sub(x, y)), Type.Int
  | Closure.Mul(x, y) -> Exp(Mul(x, y)), Type.Int
  | Closure.Div(x, y) -> Exp(Div(x, y)), Type.Int
  | Closure.IfEq(x, y, e1, e2) -> 
      let (s1, t1) = (g env e1) in
      let (s2, t2) = (g env e2) in
	assert (Type.eq t1 t2);
	(match s1, s2 with
	  | Exp(e1'), Exp(e2') -> Exp(CondEq(x, y, e1', e2')), t1
	  | _, _ -> 
	      let s1', e1' = split s1 in
	      let s2', e2' = split s2 in
		(match s1', s2' with 
		  | Some(s1'), Some(s2') -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | Some(s1'), None -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Assign(Var(z), e2'))), Exp(Var(z))))
		  | None, Some(s2') -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfEq(x, y, block (Assign(Var(z), e1')), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | None, None -> 
		      Exp(CondEq(x, y, e1', e2'))), t1)
  | Closure.IfLE(x, y, e1, e2) -> 
      let (s1, t1) = (g env e1) in
      let (s2, t2) = (g env e2) in
	assert (Type.eq t1 t2);
	(match s1, s2 with
	  | Exp(e1'), Exp(e2') -> Exp(CondLE(x, y, e1', e2')), t1
	  | _, _ -> 
	      let s1', e1' = split s1 in
	      let s2', e2' = split s2 in
		(match s1', s2' with 
		  | Some(s1'), Some(s2') -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | Some(s1'), None -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Seq(s1', Assign(Var(z), e1'))), block (Assign(Var(z), e2'))), Exp(Var(z))))
		  | None, Some(s2') -> 
		      let z = Id.gentmp t1 in 
			Seq(Dec((z, t1), None), Seq(IfLE(x, y, block (Assign(Var(z), e1')), block (Seq(s2', Assign(Var(z), e2')))), Exp(Var(z))))
		  | None, None -> Exp(CondLE(x, y, e1', e2'))), t1)
  | Closure.Let((x, t), e1, e2) -> 
      let t = k t in
      let (e1', t1) = g env e1 in
      let (e2', t2) = g (M.add x t env) e2 in
	(concat e1' (x, t) e2'), t2
  | Closure.Var(x) -> Exp(Var(x)), (M.find x env)
  | Closure.MakeCls((x, t), { Closure.entry = Id.L(l); Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: virtual_makecls) *)
      let t = k t in
      let (e2', t2) = g (M.add x t env) e2 in
      let yts = (List.map (fun y -> (y, M.find y env)) ys) in
	(make_closure (x, t) (Id.L(toplevel_fun l yts)) yts e2'), t2
  | Closure.AppCls(x, ys) -> 
      let t = (M.find x env) in
	apply_closure t (List.map (fun y -> (M.find y env)) ys);
	Exp(CallCls((x, t), ys)), (Type.apply t (List.map (fun y -> (M.find y env)) ys))
  | Closure.AppDir(Id.L(x), ys) -> 
      Exp(CallDir(Fun(x), List.map (fun y -> Var(y)) ys)), (Type.apply (M.find x env) (List.map (fun y -> M.find y env) ys))

(* 関数の仮想マシンコード生成 (caml2html: virtual_h) *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let t = k t in
  let yts = List.map (fun (y, t) -> (y, k t)) yts in
  let zts = List.map (fun (z, t) -> (z, k t)) zts in
  let body', _ = g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e in
    (match t with
      | Type.Fun(_, t2, _) ->
	  toplevel := FunDef({ name = Id.L(toplevel_fun x zts); args = yts @ zts; body = block (insert_return body'); ret = t2 }) :: !toplevel
      | _ -> assert false)
      
(* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) *)
let f (Closure.Prog(defs, e)) =
  List.iter h defs;
  predefined := M.add_list [
    ("print_int", (Type.Fun([Type.Int], Type.Unit, []), (fun x xs -> "printf(\"%d\", " ^ (string_of_exp (List.hd xs)) ^ ")")))
  ] M.empty;

  let env0 = M.fold (fun x (t, _) env -> M.add x t env) !predefined M.empty in
  let env = List.fold_left (fun env { Closure.name = (Id.L(x), t); Closure.args = _; Closure.formal_fv = _; Closure.body = _ } -> M.add x t env) env0 defs in
  let e, _ = g env e in
    (string_of_prog (Prog(List.rev !toplevel, e)))
