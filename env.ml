type t = { 
  venv   : Type.t M.t; 
  tenv  : Type.t M.t; 
  tycons : Type.tycon M.t 
}

let empty = { 
  venv   = M.empty; 
  tenv  = M.empty;
  tycons = M.add_list [("bool", Type.Bool); ("int", Type.Int)] M.empty
}

let extenv : Type.t M.t ref = ref M.empty
  
let add_var_type env x t = { env with venv = M.add x t env.venv }

