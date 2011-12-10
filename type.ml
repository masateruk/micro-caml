type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Fun of t list * t * t list (* arguments are uncurried *)
  | Var of t option ref (* 型推論であとで代入するために ref 型になっている *)
  | Closure of t * t list (* function, free variables types *)
  | Record of (string * t) list
  | Name of string * t

let rec id_of_typ = function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "i"
  | Fun _ -> "f"
  | Var _ -> "v"
  | Closure _ -> "c"
  | Record _ -> "r"
  | Name(_, t) -> id_of_typ t

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec string_of_typ = function
  | Unit -> "()"
  | Bool -> "bool"
  | Int -> "int"
  | Fun(args, ret, fv) -> "fun(" ^ (String.concat ", " (List.map string_of_typ (args @ fv))) ^ " -> " ^ (string_of_typ ret) ^ ")"
  | Var{ contents = Some(t) } -> "var(" ^ (string_of_typ t) ^ ")"
  | Var{ contents = None } -> "var(None)"
  | Closure(f, fv) -> "cls(" ^ (string_of_typ f) ^ ", " ^ (String.concat ", " (List.map string_of_typ fv)) ^ ")"
  | Record(fileds) -> "record(" ^ (String.concat ", " (List.map (fun (s, t) -> "s : " ^ (string_of_typ t)) fileds)) ^ ")"
  | Name(s, t) -> "name(" ^ (string_of_typ t) ^ ")"
      
let rec eq t1 t2 = match t1, t2 with
  | Unit, Unit -> true
  | Bool, Bool -> true
  | Int, Int -> true
  | Fun(args1, ret1, fv1), Fun(args2, ret2, fv2) -> 
      (List.length (args1 @ fv1) = List.length (args2 @ fv2)) && 
	(List.for_all2 eq (args1 @ fv1) (args2 @ fv2)) && (eq ret1 ret2)
  | Var{ contents = Some(t1') }, Var{ contents = Some(t2') } -> eq t1' t2'
  | Var _ , Var _ -> assert false
  | Closure(f1, fv1), Closure(f2, fv2) -> (eq f1 f2) && (List.length fv1 = List.length fv2) && (List.for_all2 eq fv1 fv2)
  | Record(fileds1), Record(fileds2) -> (List.length fileds1 = List.length fileds2) && List.for_all2 (fun (s1, t1) (s2, t2) -> s1 = s2 && eq t1 t2) fileds1 fileds2
  | Name(s1, t1), Name(s2, t2) -> if s1 = s2 then (assert (eq t1 t2); true) else false
  | _ -> let _ = Printf.printf "t1 = %s\nt2 = %s\n" (string_of_typ t1) (string_of_typ t2) in false

(* xs - ys *)      
let rec diff xs ys = match xs, ys with
  | xs, [] -> xs
  | [],  _ -> assert false
  | (x::xs'), (y::ys') -> assert (eq x y); diff xs' ys'

exception PartialApplication

let apply t xs = 
  let rec loop t xs = match t with
    | Fun(args, r, fv) -> 
	(match (diff (args @ fv) xs) with
	  | [] -> r
	  | _ -> raise PartialApplication)
    | Closure(f, fv) -> 
	loop f (xs @ fv)
    | Name(s, Record((s', f)::ys)) when (Str.string_match (Str.regexp "c_.*_t") s 0) ->
	loop (Closure(f, (List.map snd ys))) xs
    | Name(_, t') -> loop t' xs
    | _ -> assert false in
    try loop t xs
    with PartialApplication -> Closure(t, xs)	  

(*
let test_apply = 
  assert ((apply (Fun([Int], Int, [])) [Int]) = Int);
  assert ((apply (Fun([], Int, [])) []) = Int)
*)
