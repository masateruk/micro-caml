1type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Var of tyvar
  | Field of t * t (* レコードの型 * フィールドの型 *)
  | App of tycon * t list
  | Poly of tyvar list * t
  | Meta of t option ref (* 型推論であとで代入するために ref 型になっている *)
  | NameTy of Id.t * t option ref (* 型推論後は型環境を引かなくてもいいように定義型を入れる *)
and tycon =
  | Unit
  | Bool
  | Int
  | Arrow
  | Record of Id.t * Id.t list (* フィールド識別子のリスト *)
  | TyFun of tyvar list * t
and tyvar = Id.t
and metavar = Id.t

let counter = ref 0
let newtyvar () = 
  incr counter;
  Printf.sprintf "tyvar_%d" !counter
let newmetavar () = ref None

let rec string_of = 
  function
  | Var(v) -> "Var(" ^ v ^ ")"
  | Field(tid, t) -> "Field(" ^ (string_of tid) ^ ", " ^ (string_of t) ^ ")"
  | App(tycon, []) -> "App(" ^ (string_of_tycon tycon) ^ ", [])"
  | App(tycon, ts) -> "App(" ^ (string_of_tycon tycon) ^ ", [" ^ (String.concat "; " (List.map string_of ts)) ^ "])"
  | Poly([], t)-> "Poly([], " ^ (string_of t) ^ ")"
  | Poly(xs, t)-> "Poly(" ^ (String.concat ", " xs) ^ ", " ^ (string_of t) ^ ")"
  | Meta{ contents = Some(t) } -> "Meta(Some(" ^ (string_of t) ^ "))"
  | Meta{ contents = None } -> "Meta(none)"
  | NameTy(x, { contents = None }) -> "NameTy(" ^ x ^ ", None)"
  | NameTy(x, { contents = Some(t) }) -> "NameTy(" ^ x ^ ", Some)"
      
and string_of_tycon = 
  function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Arrow -> "Arrow"
  | Record(x, fs) -> "Record(" ^ x ^ ", {" ^ (String.concat ", " fs) ^ "})"
  | TyFun(xs, t) -> "TyFun(" ^ (String.concat ", " xs) ^ " = " ^ (string_of t)
      
let rec prefix = 
  function
  | Var _ -> "v" 
  | App(tycon, _) -> prefix_of_tycon tycon
  | Poly(_, t) -> prefix t
  | _ -> assert false
      
and prefix_of_tycon = 
  function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "n"
  | Arrow -> "pfn"
  | Record _ -> "st"
  | TyFun(_, t) -> prefix t
      
let rec ocaml_of = 
  function
  | Var _ -> "'a"
  | App(Unit, []) -> "()"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Arrow, xs) -> String.concat " -> " (List.map ocaml_of xs)
  | App(Record(_, xs), ys) -> 
      "{" ^ (String.concat ";" 
               (List.map (fun (x, y) -> x ^ " = " ^ (ocaml_of y)) (List.combine xs ys))) ^ "}"
  | Poly([], t) -> ocaml_of t      
  | _ -> assert false


