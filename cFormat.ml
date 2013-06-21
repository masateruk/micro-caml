open C

let prec = 
  function
  | Nop | Bool _ | Int _ | Struct _ | Var _ 
  | AppCls _ | AppDir _ | FieldDot _  | FieldArrow _  | Sizeof _ | Ref _ | Deref _ -> 10
  | Cast _ -> 9
  | Not _ | Neg _ -> 8
  | Mul _ | Div _ -> 7
  | Add _ | Sub _  -> 6
  | LE _ -> 5
  | Eq _ -> 4
  | And _ -> 3
  | Or _ -> 2
  | Cond _ -> 1
  | Comma _ -> 0
  | Let _ -> assert false
  
let indent depth = String.make (depth * 4) ' '
  
let rec string_of_type ?(depth = 0) ?(tags = []) = 
  function
  | CType.Void -> (indent depth) ^ "void" 
  | CType.Bool -> (indent depth) ^ "bool"
  | CType.Int -> (indent depth) ^ "int"
  | CType.Enum(name, xs) -> (indent depth) ^ "enum " ^ name ^ " {\n" ^
      (String.concat ",\n" (List.map (fun x -> (indent (depth + 1)) ^ x) xs)) ^ "\n" ^ 
    (indent depth) ^ "}"
  | CType.Fun(ty_args, ty_r) -> 
      (indent depth) ^ (string_of_type ~tags:tags ty_r) ^ " (*)(" ^
        (String.concat ", " (List.map (string_of_type ~tags:tags) ty_args)) ^ ")"
  | CType.Struct(tag, parent, xts) ->
      (indent depth) ^ "struct" ^ (if tag = "" then "" else " " ^ tag) ^ " {\n" ^ 
        (match parent with Some(t) -> (string_of_type ~depth:(depth + 1) ~tags:(tag::tags) t) ^ " base;\n" | None -> "") ^
        (String.concat ";\n" 
           (List.fold_left
              (fun s (x, t) -> 
                match t with
                | CType.Nothing -> s
                | t -> (string_of_id ~depth:(depth + 1) ~tags:(tag::tags) x t) :: s) [] (List.rev xts))) ^ ";\n" ^ 
        (indent depth) ^ "}"
  | CType.Union(xts) ->
      (indent depth) ^ "union {\n" ^ 
        (String.concat ";\n" 
           (List.fold_left
              (fun s (x, t) -> 
                match t with
                | CType.Nothing -> s
                | t -> (string_of_id ~depth:(depth + 1) ~tags:tags x t) :: s) [] (List.rev xts))) ^ ";\n" ^ 
        (indent depth) ^ "}"
  | CType.NameTy(x', _) when List.mem x' tags -> (indent depth) ^ "struct " ^ x'
  | CType.NameTy(x', _) -> (indent depth) ^ x'
  | CType.RefBase -> (indent depth) ^ "ref_base_t"
  | CType.Box -> (indent depth) ^ "sp_t"
  | CType.Pointer t -> (indent depth) ^ (string_of_type ~tags:tags t) ^ "*" 
  | CType.Nothing -> (indent depth)
      
and string_of_id ?(depth = 0) ?(tags = []) x = 
  function
  | CType.Fun(args, ret) -> 
      (indent depth) ^ (string_of_type ret) ^ " (*" ^ x ^ ")(" ^ (String.concat ", " (List.map (string_of_type ~tags:tags) args)) ^ ")" 
  | CType.Nothing -> ""
  | t -> 
      (string_of_type ~depth:depth ~tags:tags t) ^ " " ^ x
        
let rec string_of_exp outer e = 
  let inner = prec e in
  let s = 
    match e with
    | Nop -> ""
    | Bool(b) -> string_of_bool b
    | Int(i) -> string_of_int i
    | Struct(x, xes) -> "(" ^ x ^ "){" ^ (String.concat ", " (List.map (fun (x, e) -> "." ^ x ^ " = " ^ (string_of_exp inner e)) xes)) ^ "}"
    | FieldDot(e, y) -> (string_of_exp inner e) ^ "." ^ y
    | FieldArrow(e, y) -> (string_of_exp inner e) ^ "->" ^ y
    | Not(e) -> "!" ^ (string_of_exp inner e)
    | And(e1, e2) -> (string_of_exp inner e1) ^ " && " ^ (string_of_exp inner e2)
    | Or(e1, e2) -> (string_of_exp inner e1) ^ " || " ^ (string_of_exp inner e2)
    | Neg(e) -> "-" ^ (string_of_exp inner e)
    | Add(e1, e2) -> (string_of_exp inner e1) ^ " + " ^ (string_of_exp inner e2)
    | Sub(e1, e2) -> (string_of_exp inner e1) ^ " - " ^ (string_of_exp inner e2)
    | Mul(e1, e2) -> (string_of_exp inner e1) ^ " * " ^ (string_of_exp inner e2)
    | Div(e1, e2) -> (string_of_exp inner e1) ^ " / " ^ (string_of_exp inner e2)
    | Eq(e1, e2) -> (string_of_exp inner e1) ^ " == " ^ (string_of_exp inner e2)
    | LE(e1, e2) -> (string_of_exp inner e1) ^ " <= " ^ (string_of_exp inner e2)
    | Var(x) -> x
    | Cond(e, e1, e2) -> (string_of_exp inner e) ^ " ? " ^ (string_of_exp inner e1) ^ " : " ^ (string_of_exp inner e2)
    | AppCls(x, xs) ->  (string_of_exp inner x) ^ "->apply(" ^ (string_of_exp inner x) ^ ", " ^ (String.concat ", " (List.map (string_of_exp (prec Comma)) xs)) ^ ")"
    | AppDir(x, xs) ->  (string_of_exp inner x) ^ "(" ^ (String.concat ", " (List.map (string_of_exp (prec Comma)) xs)) ^ ")"
    | Let((x, t), e1, e2)  -> "let " ^ (string_of_type t) ^ " " ^ x ^ " = " ^ (string_of_exp inner e1) ^ " in " ^ (string_of_exp inner e2)
    | Sizeof(t) -> "sizeof(" ^ (string_of_type t) ^ ")"
    | Ref(e) -> "&" ^ (string_of_exp inner e)
    | Deref(e) -> "*(" ^ (string_of_exp inner e) ^ ")"
    | Cast(t, e) -> "(" ^ (string_of_type t) ^ ")" ^ (string_of_exp inner e) 
    | Comma -> assert false in
    if (inner < outer) then "(" ^ s ^ ")" else s
    
let string_of_exp = string_of_exp (-1)

let rec separator = 
  function
  | If _ -> ""
  | Seq(_, s) -> separator s
  | Block _ -> ""
  | _ -> ";"
      
let rec string_of_prog (Prog(defs)) =
  let rec string_of_statement header_depth inner_depth s =
    let padding = indent header_depth in
    let and_statement = string_of_statement 0 inner_depth in
    let and_padding = indent inner_depth in
    let newline_statement = string_of_statement (inner_depth + 1) (inner_depth + 1) in
    match s with
    | Dec((x, t), None) -> padding ^ (string_of_id x t)
    | Dec((x, t), Some(e)) -> padding ^ (string_of_id x t) ^ " = " ^ (string_of_exp e)
    | Assign(x, e) -> padding ^ (string_of_exp x) ^ " = " ^ (string_of_exp e)
    | Exp(e) -> padding ^ (string_of_exp e) 
    | If(e, s1, s2) -> padding ^ "if (" ^ (string_of_exp e) ^ ") " ^ (and_statement s1) ^ " else " ^ (and_statement s2)
    | Return(e) -> padding ^ "return " ^ (string_of_exp e)
    | Seq(s1, s2) -> (string_of_statement header_depth inner_depth s1) ^ (separator s1) ^ "\n" ^ (string_of_statement inner_depth inner_depth s2) 
    | Block([], s) -> padding ^ "{\n" ^ (newline_statement s) ^ (separator s) ^ "\n" ^ and_padding ^ "}" 
    | Block(decs, s) -> padding ^ "{\n" ^ (String.concat ";\n" (List.map (string_of_dec (inner_depth + 1)) decs)) ^ ";\n\n" ^ (newline_statement s) ^ (separator s) ^ "\n" ^ and_padding ^ "}" 
  and string_of_dec depth = 
    function
    | VarDec((x, t), None) -> (indent depth) ^ (string_of_id x t)
    | VarDec((x, t), Some(e)) -> (indent depth) ^ (string_of_id x t) ^ " = " ^ (string_of_exp e) in
  let string_of_def = 
    let string_of_statement = string_of_statement 0 0 in
    function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, used) when !used ->
        let t' = string_of_type t in
        let name' = x in
        let args' = String.concat ", " (List.map (fun (y, t) -> (string_of_id y t)) yts) in
        let body' = string_of_statement s in
          t' ^ " " ^ name' ^ "(" ^ args' ^ ")\n" ^ body' ^ "\n\n"
    | TypeDef((x, t), used) when !used ->
        "typedef " ^ (string_of_id x t) ^ ";\n\n" 
    | VarDef((x, t), e) -> 
        (match e with Exp _ -> () | _ -> assert false);
        (string_of_id x t) ^ " = " ^ (string_of_statement e) ^ ";\n\n" 
    | EnumDef(xs, used) when !used ->
        "enum {\n" ^ 
          (String.concat ",\n" (List.map (fun x -> (indent 1) ^ x) xs)) ^ "\n" ^ 
          "};\n\n" 
    | _ -> "" in
    "#include <ucaml.h>\n" ^ 
      (if !enable_gc then "#include <gc.h>\n" else "") ^ 
      "\n" ^ (String.concat "" (List.map string_of_def defs))
      
let f = string_of_prog
