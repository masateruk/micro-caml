open C

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
      | Box -> (string_of_indent depth) ^ "sp_t" ^ (plus x)
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

let rec string_of_prog (Prog(defs)) =
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
      
let f = string_of_prog
