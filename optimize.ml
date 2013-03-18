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
  List.iter 
    (function 
    | FunDef({ name = Id.L(x'); _ },  b) when x' = x -> b := true 
    | TypeDef((_, CType.Enum(_, xs)),  b) when List.mem x xs -> b := true 
    | EnumDef(xs, b) when List.mem x xs -> b := true
    | _ -> ()) 
    defs

let rec mark_ty defs t =
  List.iter 
    (fun def -> 
      match t, def with 
      | CType.NameTy(x, { contents = Some(t) }), TypeDef((x', t'), b) when x' = x && CType.identical t t' -> 
          begin
            match t with
            | CType.Void | CType.Int | CType.Bool | CType.Enum _ | CType.Box | CType.Nothing | CType.NameTy(_, { contents = None }) -> ()
            | CType.Fun(xs, y) -> List.iter (mark_ty defs) (y :: xs)
            | CType.Struct(_, _, xts) 
            | CType.Union(xts) -> List.iter (mark_ty defs) (List.map snd xts)
            | CType.NameTy(_, { contents = Some(t) }) 
            | CType.Pointer(t) -> mark_ty defs t
          end;
          b := true
      | _ -> ()) 
    defs

let rec mark_exp defs = 
  function 
  | Nop | Bool _ | Int _ -> ()
  | Struct(_, xes) -> List.iter (fun (x, e) -> mark_exp defs e) xes
  | FieldDot(e, y) 
  | FieldArrow(e, y) -> mark_exp defs e; mark_id defs y
  | Not(e) | Neg(e) -> mark_exp defs e
  | And(e1, e2) | Or(e1, e2)
  | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Eq(e1, e2) | LE(e1, e2) -> mark_exp defs e1; mark_exp defs e2
  | Cons(x, y) -> mark_id defs x; mark_id defs y
  | Var(x) -> mark_id defs x
  | Cond(e, e1, e2) -> mark_exp defs e; mark_exp defs e1; mark_exp defs e2
  | CallDir(e, es) -> mark_exp defs e; List.iter (mark_exp defs) es
  | Let _ -> assert false
  | MakeClosure(Id.L(x), y, zts) -> mark_id defs x; mark_id defs y; List.iter (fun (z, t) -> mark_id defs z; mark_ty defs t) zts
  | Sizeof(t) -> mark_ty defs t
  | Ref(e) | Deref(e) -> mark_exp defs e
  | Cast(t, e) -> mark_ty defs t; mark_exp defs e
  | Comma -> assert false

let rec mark_dec defs = 
  function
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
let rec mark defs = 
  function
  | VarDef((_, t), s) -> mark_ty defs t; mark_s defs s;
  | TypeDef _ -> ()
  | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, _) -> mark_s defs s; List.iter (fun (_, t) -> mark_ty defs t) yts; mark_ty defs t
  | EnumDef(xs, b) -> b := true (* EnumDef は int で参照されるため強制的に使用していることにする *)

(* trueとの論理積, falseとの論理和を削除 *)
let rec simplify_expr =
  function
  | Int _ | Bool _ | Nop | Cons _ | Var _ | MakeClosure _ | Sizeof _ | Comma as e -> e
  | Struct(x, yes) -> Struct(x, List.map (fun (y, e) -> (y, simplify_expr e)) yes)
  | FieldDot(e, x) -> FieldDot(simplify_expr e, x)
  | FieldArrow(e, x) -> FieldArrow(simplify_expr e, x)
  | Not(e) -> Not(simplify_expr e)
  | And(e1, e2) -> 
      begin 
        match (simplify_expr e1), (simplify_expr e2) with
        | Bool(true), e 
        | e, Bool(true) -> e
        | e1', e2' -> And(e1', e2')
      end
  | Or(e1, e2) -> 
      begin 
        match (simplify_expr e1), (simplify_expr e2) with
        | Bool(false), e -> simplify_expr e
        | e, Bool(false) -> simplify_expr e
        | e1', e2' -> Or(e1', e2')
      end
  | Neg(e) -> Neg(simplify_expr e)
  | Add(e1, e2) -> Add(simplify_expr e1, simplify_expr e2)
  | Sub(e1, e2) -> Sub(simplify_expr e1, simplify_expr e2)
  | Mul(e1, e2) -> Mul(simplify_expr e1, simplify_expr e2)
  | Div(e1, e2) -> Div(simplify_expr e1, simplify_expr e2)
  | Eq(e1, e2) -> Eq(simplify_expr e1, simplify_expr e2)
  | LE(e1, e2) -> LE(simplify_expr e1, simplify_expr e2)
  | Cond(e, e1, e2) -> Cond(simplify_expr e, simplify_expr e1, simplify_expr e2)
  | CallDir(e, es) -> CallDir(simplify_expr e, List.map simplify_expr es)
  | Let(xt, e1, e2)  -> Let(xt, simplify_expr e1, simplify_expr e2)
  | Ref(e) -> Ref(simplify_expr e)
  | Deref(e) -> Deref(simplify_expr e)
  | Cast(t, e) -> Cast(t, simplify_expr e)

let _ = 
  assert ((simplify_expr (And(Bool(true), Var("x")))) = (Var("x")));
  assert ((simplify_expr (And(Bool(true), (And(Bool(true), Var("x")))))) = (Var("x")));
  assert ((simplify_expr (And((And(Bool(true), Var("x"))), (Bool(true))))) = (Var("x")))

(* if文の条件式がtrueまたはfalseの場合はifを削除 *)
let rec simplify_s =
  function
  | Dec(xt, None) -> Dec(xt, None)
  | Dec(xt, Some(e)) -> Dec(xt, Some(simplify_expr e))
  | Assign(x, e) -> Assign(simplify_expr x, simplify_expr e)
  | Exp(e) -> Exp(simplify_expr e)
  | If(e, s1, s2) -> 
      begin 
        match simplify_expr e with
        | Bool(true) -> simplify_s s1
        | Bool(false) -> simplify_s s2
        | e -> If(e, simplify_s s1, simplify_s s2)
      end
  | Return(e) -> Return(simplify_expr e)
  | Seq(s1, s2) -> 
      begin
        match (simplify_s s1), (simplify_s s2) with
        | Exp(Nop), s 
        | s, Exp(Nop) -> s
        | s1, s2 -> Seq(s1, s2)
      end
  | Block(decs, s) -> Block(decs, simplify_s s)


let g = 
  function
  | FunDef({ name = Id.L(x); args = yts; body = s; ret = t }, b) ->
      FunDef({ name = Id.L(x);  args = yts; body = simplify_s (reduce_addref s); ret = t }, b) 
  | def -> def
	      
let f (Prog(defs)) = 
  let _ = D.printf "Optimize.f \n" in
  let defs' = List.map g defs in
    List.iter (mark defs') defs';
    Prog(defs')
