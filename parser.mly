%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let addtyp x = (x, Type.Meta(Type.newmetavar ()))
%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token AST
%token SLASH
%token CONS
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LSQUARE_BRANKET
%token RSQUARE_BRANKET
%token EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%right CONS
%left PLUS MINUS
%left AST SLASH
%right prec_unary_minus
%left prec_app

/* 開始記号の定義 */
%type <Syntax.t> sequence
%start sequence

%%

simple_exp: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN sequence RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| IDENT
    { Var($1) }
| LSQUARE_BRANKET list RSQUARE_BRANKET
    { List.fold_right (fun x xs -> Cons(x, xs)) $2 (Nil(Type.Meta(Type.newmetavar ()))) }

exp: /* 一般の式 (caml2html: parser_exp) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { Neg($2) }
| exp PLUS exp /* 足し算を構文解析するルール (caml2html: parser_add) */
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp AST exp
    { Mul($1, $3) }
| exp SLASH exp
    { Div($1, $3) }
| exp CONS exp
    { Cons($1, $3) }
| exp EQUAL exp
    { Eq($1, $3) }
| exp LESS_GREATER exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { Not(LE($3, $1)) }
| exp GREATER exp
    { Not(LE($1, $3)) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

sequence: 
| exp
    { $1 }
| LET IDENT EQUAL sequence IN sequence
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET REC fundef IN sequence
    %prec prec_let
    { LetRec($3, $5) }
| exp SEMICOLON sequence
    { Let((Id.gentmp (Type.App(Type.Unit, [])), (Type.App(Type.Unit, []))), $1, $3) }
    
fundef:
| IDENT formal_args EQUAL sequence
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

list: 
| 
    { [] }
| exp tail
    { $1 :: $2 }

tail: 
| 
    { [] }
| SEMICOLON exp tail
    { $2 :: $3 }
    
