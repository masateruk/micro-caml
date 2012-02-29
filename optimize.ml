open C

let rec flatten = function
  | Seq(s1, s2) -> (flatten s1) @ (flatten s2)
  | s -> [s]

let rec seq = function
  | [] -> Exp(Nop)
  | [s] -> s
  | s::ss -> Seq(s, seq ss)

(* 余計な add_ref と release の組を削除する *)
let rec reduce_addref s = 
  seq (reduce_addref_of_list (flatten s))

and reduce_release e s = 
  let r, ss' = reduce_release_of_list e (flatten s) in 
    r, seq (ss')

and reduce_addref_of_list = function
  | [] -> []
  | s::ss -> 
      begin
	match s with
	  | Exp(CallDir(Var("add_ref"), [e])) -> 
	      let reduced, ss' = reduce_release_of_list e ss in
		if reduced then ss' else s::ss'
	  | If(e, s1, s2) -> 
	      If(e, reduce_addref s1, reduce_addref s2) :: reduce_addref_of_list ss
	  | Seq _ -> D.printf "invalid argument"; assert false
	  | Block(decs, s') -> Block(decs, reduce_addref s') :: reduce_addref_of_list ss
	  | s -> s :: reduce_addref_of_list ss
      end
	
and reduce_release_of_list e = function
  | [] -> false, []
  | s::ss -> 
      begin
	match s with
	  | Exp(CallDir(Var("release"), [e'])) when e = e' -> true, reduce_addref_of_list ss
	  | If(e, s1, s2) -> 
	      let r1, s1' = reduce_release e s1 in
	      let r2, s2' = reduce_release e s2 in
		if r1 && r2 then
		  true, If(e, s1', s2') :: reduce_addref_of_list ss
		else
		  let r, ss' = reduce_release_of_list e ss in
		    r, If(e, s1, s2) :: ss'
	  | Seq _ -> D.printf "invalid argument"; assert false
	  | Block(decs, s') -> 
	      let r, s'' = reduce_release e s' in
		if r then 
		  r, Block(decs, s'') :: reduce_addref_of_list ss
		else
		  let r, ss' = reduce_release_of_list e ss in
		    r, Block(decs, s'') :: ss'
	  | s ->
	      let r, ss' = reduce_release_of_list e ss in
		r, (s::ss')
      end

let mark_id defs x =
  List.iter (function FunDef({ name = Id.L(x'); _ }, b) when x' = x -> b := true | _ -> ()) defs

let mark_ty defs t =
  List.iter (fun def -> match t, def with NameTy(x, t), TypeDef((x', t'), b) when x' = x && is_ty_equal t t' -> b := true | _ -> ()) defs

let rec mark_exp defs = function 
    | Nop | BoolExp _ | IntExp _ -> ()
    | Nil(t) -> mark_ty defs t
    | Not(e) | Neg(e) -> mark_exp defs e
    | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Eq(e1, e2) | LE(e1, e2) -> mark_exp defs e1; mark_exp defs e2
    | Cons(x, y) -> mark_id defs x; mark_id defs y
    | Var(x) -> mark_id defs x
    | Cond(e, e1, e2) -> mark_exp defs e; mark_exp defs e1; mark_exp defs e2
    | CallDir(e, es) -> mark_exp defs e; List.iter (mark_exp defs) es
    | Let _ -> assert false
    | MakeClosure(Id.L(x), y, zts) -> mark_id defs x; mark_id defs y; List.iter (fun (z, t) -> mark_id defs z; mark_ty defs t) zts
    | Field(x, y) -> mark_id defs x; mark_id defs y
    | Sizeof(t) -> mark_ty defs t
    | Ref(e) | Deref(e) -> mark_exp defs e
    | Cast(t, e) -> mark_ty defs t; mark_exp defs e
    | Comma -> assert false

let rec mark_dec defs = function
    | VarDec((x, t), None) -> mark_id defs x; mark_ty defs t
    | VarDec((x, t), Some(e)) -> mark_id defs x; mark_ty defs t; mark_exp defs e

let rec mark_s defs = function
  | Dec((x, t), None) -> mark_id defs x; mark_ty defs t
  | Dec((x, t), Some(e)) -> mark_id defs x; mark_ty defs t; mark_exp defs e
  | Assign(e1, e2) -> mark_exp defs e1; mark_exp defs e2
  | Exp(e) -> mark_exp defs e
  | If(e, s1, s2) -> mark_exp defs e; mark_s defs s1; mark_s defs s2;
  | Return(e) -> mark_exp defs e
  | Seq(s1, s2) -> mark_s defs s1; mark_s defs s2
  | Block(decs, s) -> List.iter (mark_dec defs) decs; mark_s defs s

(* 関数定義で使用している定義にマークをつける。不要な定義を出力しないため。ただし他から呼ばれない再帰関数は使用ありとマークされるなど不完全。*)      
let rec mark defs = function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, _) -> mark_s defs s; List.iter (fun (_, t) -> mark_ty defs t) yts; mark_ty defs t
    | def -> ()

let g = function
    | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, b) ->
	FunDef({ name = Id.L(x);  args = yts; body = reduce_addref s; ret = t }, b)
    | def -> def
	      
let f (Prog(defs)) = 
  let defs' = List.map g defs in
    List.iter (mark defs') defs';
    Prog(defs')
