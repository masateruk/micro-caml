type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
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
  | Tuple
  | Record of Id.t * Id.t list (* 型名とフィールド識別子のリスト。型名はあとで名前引きやすいようにするため *)
  | Variant of Id.t * (Id.t * t list) list (* 最初の要素は型名。理由は同上 *)
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
  | App(tycon, ts) -> "App(" ^ (string_of_tycon tycon) ^ ", [" ^ (String.concat "; " (List.map string_of ts)) ^ "])"
  | Poly([], t)-> "Poly([], " ^ (string_of t) ^ ")"
  | Poly(xs, t)-> "Poly(" ^ (String.concat ", " xs) ^ ", " ^ (string_of t) ^ ")"
  | Meta{ contents = Some(t) } -> "Meta(Some(" ^ (string_of t) ^ "))"
  | Meta{ contents = None } -> "Meta(None)"
  | NameTy(x, { contents = None }) -> "NameTy(" ^ x ^ ", None)"
  | NameTy(x, { contents = Some(t) }) -> "NameTy(" ^ x ^ ", Some(" ^ (string_of t) ^ "))"
      
and string_of_tycon = 
  function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Arrow -> "Arrow"
  | Tuple -> "Tuple"
  | Record(x, fs) -> "Record(" ^ x ^ ", {" ^ (String.concat ", " fs) ^ "})"
  | Variant(x, ytss) -> "Variant(" ^ x ^ ", " ^
      (String.concat " | " 
         (List.map 
            (fun (y, ts) -> y ^ 
              (match ts with 
              | [] -> "" 
              | ts -> " of " ^ (String.concat " * " (List.map string_of ts)))) ytss)) ^ ")"
  | TyFun(xs, t) -> "TyFun(" ^ (String.concat ", " xs) ^ " = " ^ (string_of t) ^ ")"
      
let rec prefix = 
  function
  | Var _ -> "v" 
  | App(tycon, _) -> prefix_of_tycon tycon
  | Poly(_, t) -> prefix t
  | NameTy(_, { contents = Some(t) }) -> prefix t
  | _ -> assert false
      
and prefix_of_tycon = 
  function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "n"
  | Arrow -> "pfn"
  | Tuple -> "t"
  | Record _ -> "st"
  | Variant _ -> "v"
  | TyFun(_, t) -> prefix t
      
let rec ocaml_of = 
  function
  | Var _ -> "'a"
  | App(Unit, []) -> "()"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Arrow, xs) -> String.concat " -> " (List.map ocaml_of xs)
  | App(Tuple, xs) -> "(" ^ (String.concat " * " (List.map ocaml_of xs)) ^ ")"
  | App(Record(_, xs), ys) -> 
      "{" ^ (String.concat ";" 
               (List.map (fun (x, y) -> x ^ " = " ^ (ocaml_of y)) (List.combine xs ys))) ^ "}"
  | App(Variant(_, ytss), []) -> 
      (String.concat " | " 
         (List.map 
            (fun (y, ts) -> y ^ 
              (match ts with 
              | [] -> "" 
              | ts -> " of " ^ (String.concat " * " (List.map ocaml_of ts)))) ytss))
  | Poly([], t) -> ocaml_of t      
  | NameTy(_, { contents = Some(t) }) -> ocaml_of t
  | NameTy(x, { contents = None }) -> x
  | t -> Printf.eprintf "%s : not implemented yet." (string_of t); assert false

(* 等値判定。型推論後のみ使用可能。*)
let rec equal t1 t2 = 
  match t1, t2 with
  | App(Unit, xs), App(Unit, ys) 
  | App(Bool, xs), App(Bool, ys) 
  | App(Int, xs), App(Int, ys) 
  | App(Arrow, xs), App(Arrow, ys) when List.length xs = List.length ys -> List.for_all2 equal xs ys
  | App(Record(x, _), xs), App(Record(y, _), ys) 
  | App(Variant(x, _), xs), App(Variant(y, _), ys) when List.length xs = List.length ys -> 
      x = y && List.for_all2 equal xs ys
  | App(TyFun(xs, u), ys), t2 -> assert false (* inpossible after Typing.f *)
  | Poly([], u1), t2 -> equal u1 t2
  | t1, Poly([], u2) -> equal t1 u2
  | Poly(xs, u1), Poly(ys, u2) -> assert false (* inpossible after Typing.f *) 
  | Var(x), Var(y) when x = y -> true
  | Field(_, x), Field(_, y) -> equal x y
  | Meta{ contents = Some(t1') }, t2 -> equal t1' t2
  | Meta(x), Meta{ contents = Some(t2') } -> equal t1 t2'
  | Meta(x), Meta(y) when x == y -> true
  | Meta(x), t2 -> assert false (* inpossible after Typing.f *)
  | t1, Meta(y) -> equal t2 t1
  | NameTy(_, { contents = Some(t1') }), t2 -> equal t1' t2
  | t1, NameTy(_, { contents = Some(t2') }) -> equal t2' t1
  | NameTy(_, _), _ | _, NameTy(_, _) -> assert false (* inpossible after Typing.f *)
  | _, _ -> false
      
let rec apply ft argts =
  match ft with
  | App(Arrow, xs) -> 
      assert (L.is_sub equal argts xs);
      (match L.sub equal xs argts with
      | [] -> assert false (* impossible *)
      | [t] -> t
      | t::ts' as ts -> App(Arrow, ts))
  | NameTy(_, { contents = Some(t) }) -> apply t argts
  | _ -> assert false

(* 型環境 venv に追加する識別子と型のリスト *)
let rec ids t =
  let () = D.printf "Type.ids %s\n" (string_of t) in
  match t with
  | App(Variant(x, ytss), []) -> 
      let t' = NameTy(x, { contents = Some(t) }) in 
      (x, t) :: (List.map (fun (y, ts) -> y, 
        (match ts with
        | [] -> App(Int, [])
        | _ -> App(Arrow, ts @ [t']))) ytss)
  | App(Variant(x, ytss), ts) -> assert false (* not implemented yet. *)
  | _ -> []
      
(* 型環境 tenv に追加する識別子と型のリスト *)
let rec types t =
  let () = D.printf "Types.types %s\n" (string_of t) in
  match t with
  | App(Record(x, fs), ys) -> (x, t) :: (List.combine fs (List.map (fun y -> Field(t, y)) ys))
  | _ -> []

let rec name t =
  match t with
  | App(Unit, []) -> "unit"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Arrow, ts) -> "fun_of_" ^ (String.concat "_" (List.map name ts))
  | App(Tuple, ts) -> "tuple_of_" ^ (String.concat "_" (List.map name ts))
  | App(Record(x, _), _) -> x
  | App(Variant(x, _), []) -> x
  | Field(_, t) -> name t
  | NameTy(x, _) -> assert(x <> ""); x
  | Var _ | Poly _ | Meta _ | App(Unit, _) | App(Bool, _) | App(Int, _) | App(TyFun _, _) -> assert false (* impossible *)
  | App(Variant(_, _), _) -> D.printf "t = %s\n" (string_of t); assert false (* not implemented yet *)
      
