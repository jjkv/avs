type token =
  | EOF
  | EOL
  | COMMA
  | CONST
  | MOV
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | LT
  | LEQ
  | IS_INT
  | IS_STR
  | IS_TAB
  | JMP
  | IF_ZERO
  | RD_GLOB
  | WR_GLOB
  | MK_TAB
  | RD_TAB
  | WR_TAB
  | HAS_TAB
  | CALL
  | RET
  | HALT
  | INT_SYM
  | ASSERT_ZERO
  | INT of (int)
  | REG of (int)
  | HOLE of (int)
  | STR of (string)
  | ID of (string)
  | FN of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Instr
# 42 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* EOL *);
  258 (* COMMA *);
  259 (* CONST *);
  260 (* MOV *);
  261 (* ADD *);
  262 (* SUB *);
  263 (* MUL *);
  264 (* DIV *);
  265 (* EQ *);
  266 (* LT *);
  267 (* LEQ *);
  268 (* IS_INT *);
  269 (* IS_STR *);
  270 (* IS_TAB *);
  271 (* JMP *);
  272 (* IF_ZERO *);
  273 (* RD_GLOB *);
  274 (* WR_GLOB *);
  275 (* MK_TAB *);
  276 (* RD_TAB *);
  277 (* WR_TAB *);
  278 (* HAS_TAB *);
  279 (* CALL *);
  280 (* RET *);
  281 (* HALT *);
  282 (* INT_SYM *);
  283 (* ASSERT_ZERO *);
    0|]

let yytransl_block = [|
  284 (* INT *);
  285 (* REG *);
  286 (* HOLE *);
  287 (* STR *);
  288 (* ID *);
  289 (* FN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\001\000\001\000\002\000\003\000\002\000\
\002\000\003\000\003\000\002\000\004\000\004\000\004\000\004\000\
\004\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\004\000\004\000\002\000\004\000\004\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\031\000\001\000\000\000\005\000\
\003\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\030\000\012\000\000\000\009\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\010\000\013\000\014\000\
\015\000\016\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\026\000\028\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\027\000\028\000"

let yysindex = "\004\000\
\255\254\000\000\255\254\016\255\000\000\000\000\245\254\000\000\
\000\000\030\255\000\000\253\254\000\255\001\255\002\255\015\255\
\019\255\020\255\021\255\022\255\023\255\024\255\027\255\028\255\
\029\255\031\255\003\255\002\000\026\255\054\255\057\255\059\255\
\060\255\061\255\062\255\063\255\064\255\065\255\066\255\000\000\
\067\255\068\255\000\000\000\000\003\000\000\000\000\000\249\254\
\042\255\043\255\044\255\045\255\046\255\047\255\048\255\049\255\
\050\255\051\255\053\255\052\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\080\255\081\255\083\255\084\255\085\255\
\086\255\087\255\000\000\000\000\000\000\000\000\069\255\070\255\
\071\255\072\255\073\255\074\255\075\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\090\000\000\000\090\000\000\000\000\000\000\000\091\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\089\000\000\000\086\000\000\000\067\000"

let yytablesize = 290
let yytable = "\003\000\
\007\000\046\000\061\000\044\000\001\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\010\000\023\000\024\000\025\000\063\000\004\000\064\000\065\000\
\066\000\029\000\026\000\048\000\030\000\031\000\032\000\004\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\033\000\023\000\024\000\025\000\034\000\
\035\000\036\000\037\000\038\000\039\000\026\000\040\000\049\000\
\041\000\042\000\050\000\043\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\079\000\080\000\078\000\081\000\082\000\083\000\084\000\
\085\000\002\000\004\000\009\000\011\000\045\000\000\000\000\000\
\000\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\062\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000"

let yycheck = "\001\001\
\000\000\000\000\000\000\001\001\001\000\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\001\001\015\001\016\001\017\001\028\001\033\001\030\001\031\001\
\032\001\029\001\024\001\002\001\029\001\029\001\029\001\033\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\029\001\015\001\016\001\017\001\029\001\
\029\001\029\001\029\001\029\001\029\001\024\001\028\001\002\001\
\029\001\029\001\002\001\029\001\002\001\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\002\001\002\001\029\001\029\001\
\029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
\028\001\002\001\002\001\032\001\002\001\002\001\002\001\002\001\
\002\001\000\000\000\000\003\000\007\000\027\000\255\255\255\255\
\255\255\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\001\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\033\001"

let yynames_const = "\
  EOF\000\
  EOL\000\
  COMMA\000\
  CONST\000\
  MOV\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  EQ\000\
  LT\000\
  LEQ\000\
  IS_INT\000\
  IS_STR\000\
  IS_TAB\000\
  JMP\000\
  IF_ZERO\000\
  RD_GLOB\000\
  WR_GLOB\000\
  MK_TAB\000\
  RD_TAB\000\
  WR_TAB\000\
  HAS_TAB\000\
  CALL\000\
  RET\000\
  HALT\000\
  INT_SYM\000\
  ASSERT_ZERO\000\
  "

let yynames_block = "\
  INT\000\
  REG\000\
  HOLE\000\
  STR\000\
  ID\000\
  FN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 15 "parser.mly"
       ( List.rev _1 )
# 269 "parser.ml"
               : (string * Instr.instr list) list))
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "parser.mly"
  ( [] )
# 275 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 19 "parser.mly"
           ( _2 )
# 282 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'progp) in
    Obj.repr(
# 20 "parser.mly"
        ( _1 )
# 289 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fn) in
    Obj.repr(
# 23 "parser.mly"
     ( [_1] )
# 296 "parser.ml"
               : 'progp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'progp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fn) in
    Obj.repr(
# 24 "parser.mly"
           ( _2::_1 )
# 304 "parser.ml"
               : 'progp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 27 "parser.mly"
                ( _1, List.rev _3 )
# 312 "parser.ml"
               : 'fn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 30 "parser.mly"
            ( [_1] )
# 319 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 31 "parser.mly"
            ( [_1] )
# 326 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instrs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 32 "parser.mly"
                   ( _2::_1 )
# 334 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instrs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 33 "parser.mly"
                   ( _2::_1 )
# 342 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 34 "parser.mly"
             ( _1 )
# 349 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 37 "parser.mly"
                      ( I_const (`L_Reg _2, `L_Int _4) )
# 357 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parser.mly"
                       ( I_const (`L_Reg _2, `L_Hole _4) )
# 365 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                      ( I_const (`L_Reg _2, `L_Str _4) )
# 373 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "parser.mly"
                     ( I_const (`L_Reg _2, `L_Id _4) )
# 381 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "parser.mly"
                    ( I_mov (`L_Reg _2, `L_Reg _4) )
# 389 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                              ( I_add (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 398 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
                              ( I_sub (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 407 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                              ( I_mul (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 416 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                              ( I_div (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 425 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                             ( I_eq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 434 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 47 "parser.mly"
                             ( I_lt (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 443 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
                              ( I_leq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 452 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
                       ( I_is_int (`L_Reg _2, `L_Reg _4) )
# 460 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser.mly"
                       ( I_is_str (`L_Reg _2, `L_Reg _4) )
# 468 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
          ( I_jmp _2 )
# 475 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
                        ( I_if_zero (`L_Reg _2, _4) )
# 483 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                       ( I_rd_glob (`L_Reg _2, `L_Id _4) )
# 491 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
          ( I_ret (`L_Reg _2) )
# 498 "parser.ml"
               : 'instr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * Instr.instr list) list)
