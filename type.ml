type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Var of tyvar
  | Field of t * t (* レコードの型 * フィールドの型 *)
  | Variant of Id.t * (Id.t * t list) list (* 最初の要素は型名。理由は同上 *)
  | App of tycon * t list
  | Poly of tyvar list * t
  | Meta of t option ref (* 型推論であとで代入するために ref 型になっている *)
and tycon =
  | Unit
  | Bool
  | Int
  | Arrow
  | Tuple
  | Record of Id.t * Id.t list (* 型名とフィールド識別子のリスト。型名はあとで名前引きやすいようにするため *)
  | TyFun of tyvar list * t
  | NameTycon of Id.t * tycon option ref 
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
  | Variant(x, ytss) -> "Variant(" ^ x ^ ", " ^
      (String.concat " | " 
         (List.map 
            (fun (y, ts) -> y ^ 
              (match ts with 
              | [] -> "" 
              | ts -> " of " ^ (String.concat " * " (List.map string_of ts)))) ytss)) ^ ")"
  | App(tycon, ts) -> "App(" ^ (string_of_tycon tycon) ^ ", [" ^ (String.concat "; " (List.map string_of ts)) ^ "])"
  | Poly([], t)-> "Poly([], " ^ (string_of t) ^ ")"
  | Poly(xs, t)-> "Poly(" ^ (String.concat ", " xs) ^ ", " ^ (string_of t) ^ ")"
  | Meta{ contents = Some(t) } -> "Meta(Some(" ^ (string_of t) ^ "))"
  | Meta{ contents = None } -> "Meta(None)"
      
and string_of_tycon = 
  function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Arrow -> "Arrow"
  | Tuple -> "Tuple"
  | Record(x, fs) -> "Record(" ^ x ^ ", {" ^ (String.concat ", " fs) ^ "})"
  | TyFun(xs, t) -> "TyFun(" ^ (String.concat ", " xs) ^ ", " ^ (string_of t) ^ ")"
  | NameTycon(x, { contents = None }) -> "NameTycon(" ^ x ^ ", None)"
  | NameTycon(x, { contents = Some(t) }) -> "NameTycon(" ^ x ^ ", Some(" ^ (string_of_tycon t) ^ "))"
      
let rec prefix = 
  function
  | Var _ -> "v" 
  | Field(_, t) -> prefix t
  | Variant _ -> "v"
  | App(tycon, _) -> prefix_of_tycon tycon
  | Poly(_, t) -> prefix t
  | t -> D.printf "t = %s\n" (string_of t); assert false
      
and prefix_of_tycon = 
  function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "n"
  | Arrow -> "pfn"
  | Tuple -> "t"
  | Record _ -> "st"
  | TyFun(_, t) -> prefix t
  | NameTycon(x, _) -> x
      
let rec ocaml_of = 
  function
  | Var _ -> "'a"
  | Field(_, t) -> ocaml_of t
  | Variant(_, ytss) -> 
      (String.concat " | " 
         (List.map 
            (fun (y, ts) -> y ^ 
              (match ts with 
              | [] -> "" 
              | ts -> " of " ^ (String.concat " * " (List.map ocaml_of ts)))) ytss))
  | App(Unit, []) -> "()"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Arrow, xs) -> String.concat " -> " (List.map ocaml_of xs)
  | App(Tuple, xs) -> "(" ^ (String.concat " * " (List.map ocaml_of xs)) ^ ")"
  | App(Record(_, xs), ys) -> 
      "{" ^ (String.concat ";" 
               (List.map (fun (x, y) -> x ^ " = " ^ (ocaml_of y)) (List.combine xs ys))) ^ "}"
  | Poly(xs, t) -> ocaml_of t      
  | App(TyFun([], t), []) -> ocaml_of t
  | App(NameTycon(x, _), ts) -> (String.concat " * " (List.map ocaml_of ts)) ^ " " ^ x
  | t -> Printf.eprintf "%s : not implemented yet." (string_of t); assert false

(* 等値判定。型推論後のみ使用可能。*)
let rec equal t1 t2 = 
  match t1, t2 with
  | App(Unit, xs), App(Unit, ys) 
  | App(Bool, xs), App(Bool, ys) 
  | App(Int, xs), App(Int, ys) 
  | App(Arrow, xs), App(Arrow, ys) 
  | App(Tuple, xs), App(Tuple, ys) when List.length xs = List.length ys -> List.for_all2 equal xs ys
  | App(Record(x, _), xs), App(Record(y, _), ys) when List.length xs = List.length ys -> x = y && List.for_all2 equal xs ys
  | App(TyFun(xs, u), ys), t2 -> assert false (* inpossible after Typing.f *)
  | Poly([], u1), t2 -> equal u1 t2
  | t1, Poly([], u2) -> equal t1 u2
  | Poly(xs, u1), Poly(ys, u2) -> xs = ys && equal u1 u2
  | Var(x), Var(y) -> true
  | Field(_, x), Field(_, y) -> equal x y
  | Variant(x, _), Variant(y, _) -> x = y
  | Meta{ contents = Some(t1') }, t2 -> equal t1' t2
  | Meta(x), Meta{ contents = Some(t2') } -> equal t1 t2'
  | Meta(x), Meta(y) when x == y -> true
  | Meta(x), t2 -> assert false (* inpossible after Typing.f *)
  | t1, Meta(y) -> equal t2 t1
  | _, _ -> false
      
let rec apply ft argts =
  match ft with
  | App(Arrow, xs) -> 
      assert (L.is_sub equal argts xs);
      (match L.sub equal xs argts with
      | [] -> assert false (* impossible *)
      | [t] -> t
      | t::ts' as ts -> App(Arrow, ts))
  | _ -> assert false

(* 型環境 tycons に追加する識別子と型のリスト *)
let rec tycons t =
  let () = D.printf "Type.tycons %s\n" (string_of_tycon t) in
  match t with
  | TyFun(xs, (Variant(x, ytss) as t)) -> 
      (List.map (fun (y, ts) -> y, 
        (match ts with
        | [] -> TyFun(xs, t)
        | _ -> TyFun(xs, App(Arrow, ts @ [t])))) ytss)
  | _ -> []

(* 型環境 tenv に追加する識別子と型のリスト *)
let rec types t =
  let () = D.printf "Types.types %s\n" (string_of_tycon t) in
  match t with
  | TyFun(xs, (App(Record(x, fs), ys) as t)) -> (List.combine fs (List.map (fun y -> (Poly(xs, Field(t, y))))  ys))
  | _ -> []

let rec name t =
  match t with
  | App(Unit, []) -> "unit"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Arrow, ts) -> "fun_of_" ^ (String.concat "_" (List.map name ts))
  | App(Tuple, ts) -> "tuple_of_" ^ (String.concat "_" (List.map name ts))
  | App(Record(x, _), _) -> x
  | Field(_, t) -> name t
  | Variant(x, _) -> x
  | App(TyFun([], t), []) -> name t
  | Var _ | Poly _ | Meta _ | App(Unit, _) | App(Bool, _) | App(Int, _) | App(TyFun _, _) -> assert false (* impossible *)
  | App(NameTycon(x, _), _) -> x


