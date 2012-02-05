let verbose = ref false
    
let printf format =
  if !verbose then
    Printf.printf format
  else
    Printf.ifprintf stdout format

