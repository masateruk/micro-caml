open C

let prec = 
  function
  | Nop | Nil _ | Bool _ | Int _ | Struct _ | Var _ 
  | CallDir _ | MakeClosure _ | Field _  | Sizeof _ | Ref _ | Deref _ | Cast _ -> 7
  | Not _ | Neg _ -> 6
  | Mul _ | Div _ -> 5
  | Add _ | Sub _  -> 4
  | LE _ -> 3
  | Eq _ -> 2
  | Cond _ -> 1
  | Comma _ -> 0
  | Cons _ -> assert false
  | Let _ -> assert false
  
let indent depth = String.make (depth * 2) ' '
  
let rec string_of_type ?(depth = 0) ?(tags = []) = 
  function
  | CType.Void -> (indent depth) ^ "void" 
  | CType.Bool -> (indent depth) ^ "bool"
  | CType.Int -> (indent depth) ^ "int"
  | CType.Fun _ -> assert false
  | CType.Struct(tag, xts) ->
      (indent depth) ^ "struct" ^ (if tag = "" then "" else " " ^ tag) ^ " {\n" ^ 
        (String.concat ";\n" (List.map (fun (x, t) -> string_of_id ~depth:(depth + 1) ~tags:(tag::tags) x t) xts)) ^ ";\n" ^ 
        (indent depth) ^ "}"
  | CType.NameTy(x', _) when List.mem x' tags -> (indent depth) ^ "struct " ^ x'
  | CType.NameTy(x', _) -> (indent depth) ^ x'
  | CType.Box -> (indent depth) ^ "sp_t"
  | CType.Pointer t -> (indent depth) ^ (string_of_type ~tags:tags t) ^ "*" 
      
and string_of_id ?(depth = 0) ?(tags = []) x = 
  function
  | CType.Fun(args, ret) -> 
      (indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map (string_of_type ~tags:tags) args)) ^ ")" 
  | t -> 
      (string_of_type ~depth:depth ~tags:tags t) ^ " " ^ x
        
let rec string_of_exp outer e = 
  let inner = prec e in
  let s = 
    match e with
    | Nop -> ""
    | Nil _ -> "nil"
    | Bool(b) -> string_of_bool b
    | Int(i) -> string_of_int i
    | Struct(x, xes) -> "(" ^ x ^ "){" ^ (String.concat ", " (List.map (fun (x, e) -> "." ^ x ^ " = " ^ (string_of_exp inner e)) xes)) ^ "}"
    | Field(e, y) -> (string_of_exp inner e) ^ "." ^ y
    | Not(e) -> "!" ^ (string_of_exp inner e)
    | Neg(e) -> "-" ^ (string_of_exp inner e)
    | Add(e1, e2) -> (string_of_exp inner e1) ^ " + " ^ (string_of_exp inner e2)
    | Sub(e1, e2) -> (string_of_exp inner e1) ^ " - " ^ (string_of_exp inner e2)
    | Mul(e1, e2) -> (string_of_exp inner e1) ^ " * " ^ (string_of_exp inner e2)
    | Div(e1, e2) -> (string_of_exp inner e1) ^ " / " ^ (string_of_exp inner e2)
    | Eq(e1, e2) -> (string_of_exp inner e1) ^ " == " ^ (string_of_exp inner e2)
    | LE(e1, e2) -> (string_of_exp inner e1) ^ " <= " ^ (string_of_exp inner e2)
    | Cons(x, y) -> x ^ " :: " ^ y
    | Var(x) -> x
    | Cond(e, e1, e2) -> (string_of_exp inner e) ^ " ? " ^ (string_of_exp inner e1) ^ " : " ^ (string_of_exp inner e2)
    | CallDir(x, xs) ->  (string_of_exp inner x) ^ "(" ^ (String.concat ", " (List.map (string_of_exp (prec Comma)) xs)) ^ ")"
    | Let((x, t), e1, e2)  -> "let " ^ (string_of_type t) ^ " " ^ x ^ " = " ^ (string_of_exp inner e1) ^ " in " ^ (string_of_exp inner e2)
    | MakeClosure(Id.L(l), x, yts) -> l ^ "(" ^ x ^ ", " ^ (String.concat ", " (List.map fst yts)) ^ ")" 
    | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
    | Ref(e) -> "&" ^ (string_of_exp inner e)
    | Deref(e) -> "*(" ^ (string_of_exp inner e) ^ ")"
    | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_exp inner e) 
    | Comma -> assert false in
    if (inner < outer) then "(" ^ s ^ ")" else s
    
let rec string_of_separator = 
  function
  | If _ -> ""
  | Seq(_, s) -> string_of_separator s
  | Block _ -> ""
  | _ -> ";"
      
let rec string_of_prog (Prog(defs)) =
  let rec string_of_statement depth = 
    function
    | Dec((x, t), None) -> (indent depth) ^ (string_of_id x t)
    | Dec((x, t), Some(e)) -> (indent depth) ^ (string_of_id x t) ^ " = " ^ (string_of_exp (-1) e)
    | Assign(x, e) -> (indent depth) ^ (string_of_exp (-1) x) ^ " = " ^ (string_of_exp (-1) e)
    | Exp(e) -> (indent depth) ^ (string_of_exp (-1) e) 
    | If(e, s1, s2) -> (indent depth) ^ "if (" ^ (string_of_exp (-1) e) ^ ") " ^ (string_of_statement depth s1) ^ " else " ^ (string_of_statement depth s2)
    | Return(e) -> (indent depth) ^ "return " ^ (string_of_exp (-1) e)
    | Seq(s1, s2) -> (string_of_statement depth s1) ^ (string_of_separator s1) ^ "\n" ^ (string_of_statement depth s2) 
    | Block([], s) -> "{\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (indent depth) ^ "}" 
    | Block(decs, s) -> "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (depth + 1)) decs)) ^ ";\n\n" ^ (string_of_statement (depth + 1) s) ^ ";\n" ^ (indent depth) ^ "}" 
  and string_of_dec depth = 
    function
    | VarDec((x, t), None) -> (indent depth) ^ (string_of_id x t)
    | VarDec((x, t), Some(e)) -> (indent depth) ^ (string_of_id x t) ^ " = " ^ (string_of_exp (-1) e) in
  let string_of_def = 
    function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, used) when !used ->
        let t' = string_of_type t in
        let name' = x in
        let args' = String.concat ", " (List.map (fun (y, t) -> (string_of_id y t)) yts) in
        let body' = string_of_statement 0 s in
          t' ^ " " ^ name' ^ "(" ^ args' ^ ")\n" ^ body' ^ "\n\n"
    | TypeDef((x, t), used) when !used ->
        "typedef " ^ (string_of_id x t) ^ ";\n\n" 
    | VarDef((x, t), e) -> 
        (match e with Exp _ -> () | _ -> assert false);
        (string_of_id x t) ^ " = " ^ (string_of_statement 0 e) ^ ";\n\n" 
    | _ -> "" in
    "#include <ucaml.h>\n" ^ 
      (if !enable_gc then "#include <gc.h>\n" else "") ^ 
      "\n" ^ (String.concat "" (List.map string_of_def defs))
      
let f = string_of_prog
