type t = { 
  venv   : Type.t M.t; 
  types  : Type.t M.t; 
  tycons : Type.tycon M.t 
}

let empty = { 
  venv   = M.empty; 
  types  = M.empty;
  tycons = M.add_list [("bool", Type.Bool); ("int", Type.Int)] M.empty
}

let extenv : Type.t M.t ref = ref M.empty
  
let add_var_type env x t = { env with venv = M.add x t env.venv }

let find_rec_tycon { venv = venv; types = types; tycons = tycons } field = 
  match M.find field types with
  | Type.Poly(xs, Type.Field(x, _)) -> x
  | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
