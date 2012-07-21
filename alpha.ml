(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x ids = try M.find x ids with Not_found -> x
let genid x ids = if (M.mem x ids) then Id.genid x else x
let add x y ids = if (M.mem x ids) then ids else (M.add x y ids)
let add_list xs ids = List.fold_left (fun ids x -> add x (genid x ids) ids) ids xs

let rec h ids = function
  | Bool(b) -> Bool(b)
  | Int(i) -> Int(i)
  | Record(xs) -> Record(List.map (fun (x, e) -> find x ids, (h ids e)) xs)
  | Field(e, x) -> Field(h ids e, find x ids)
  | Tuple(es) -> Tuple(List.map (h ids) es)
  | Not(e) -> Not(h ids e)
  | Var(x) -> Var(find x ids)
  | Constr(x, es) -> Constr(find x ids, List.map (h ids) es)
  | Neg(e) -> Neg(h ids e)
  | Add(e1, e2) -> Add(h ids e1, h ids e2)
  | Sub(e1, e2) -> Sub(h ids e1, h ids e2)
  | Mul(e1, e2) -> Mul(h ids e1, h ids e2)
  | Div(e1, e2) -> Div(h ids e1, h ids e2)
  | Eq(e1, e2) -> Eq(h ids e1, h ids e2)
  | LE(e1, e2) -> LE(h ids e1, h ids e2)
  | App(e, ys) -> App(h ids e, List.map (h ids) ys)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (h ids) ys)
    
let rec g ids = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Nil(t) -> Nil(t)
  | Exp(e) -> Exp(h ids e)
  | Cons(x, y) -> Cons(find x ids, find y ids)
  | If(e, e1, e2) -> If(h ids e, g ids e1, g ids e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = genid x ids in
      Let((x', t), g ids e1, g (add x x' ids) e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let ids = add x (genid x ids) ids in
      let ys = List.map fst yts in
      let ids' = add_list ys ids in
      LetRec({ name = (find x ids, t);
	       args = List.map (fun (y, t) -> (find y ids', t)) yts;
	       body = g ids' e1 },
	     g ids e2)
  | WrapBody(x, t) -> WrapBody(x, t)
  | UnwrapBody(x, t) -> UnwrapBody(x, t)
      
let f =
  let f' (ids, defs) =
    function
    | TypeDef(x, t) -> 
        (add_list (List.map fst (Type.ids t)) ids),  TypeDef(x, t) :: defs
    | VarDef((x, t), e) -> 
        (add x (genid x ids) ids), VarDef((x, t), g ids e) :: defs
    | RecDef({ name = (x, t); args = yts; body = e1 }) -> 
        (add x (genid x ids) ids), RecDef({ name = (x, t); args = yts; body = g ids e1 }) :: defs in
  fold f' M.empty
