type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "_%s%d" s !counter

let gentmp = genid

let to_upper s = 
  String.uppercase (Str.global_replace (Str.regexp "\\([a-z0-9]\\)\\([A-Z]+\\)") "\\1_\\2" s)

let to_lower s =
  String.concat "" (List.map (fun s -> String.capitalize (String.lowercase s)) (Str.split (Str.regexp "_") s))
