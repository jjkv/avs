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
  | STR of (string)
  | ID of (string)
  | FN of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Instr
# 41 "parser.ml"
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
  286 (* STR *);
  287 (* ID *);
  288 (* FN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\001\000\001\000\002\000\003\000\002\000\
\002\000\003\000\003\000\002\000\004\000\004\000\004\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\004\000\
\004\000\002\000\004\000\006\000\002\000\004\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\032\000\001\000\000\000\005\000\
\003\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\029\000\000\000\031\000\012\000\
\000\000\009\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\010\000\013\000\014\000\015\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\025\000\
\027\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\028\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\029\000\030\000"

let yysindex = "\004\000\
\255\254\000\000\255\254\016\255\000\000\000\000\245\254\000\000\
\000\000\029\255\000\000\251\254\252\254\014\255\017\255\018\255\
\019\255\020\255\021\255\022\255\025\255\028\255\000\255\030\255\
\031\255\032\255\033\255\034\255\003\255\002\000\056\255\062\255\
\063\255\064\255\065\255\066\255\067\255\068\255\069\255\070\255\
\071\255\000\000\072\255\073\255\000\000\074\255\000\000\000\000\
\003\000\000\000\000\000\248\254\048\255\049\255\050\255\051\255\
\052\255\053\255\054\255\055\255\057\255\058\255\060\255\061\255\
\059\255\000\000\000\000\000\000\000\000\000\000\000\000\083\255\
\089\255\090\255\091\255\092\255\093\255\094\255\000\000\000\000\
\000\000\095\255\000\000\075\255\076\255\077\255\078\255\079\255\
\080\255\081\255\084\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\098\000\000\000\098\000\000\000\000\000\000\000\099\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\097\000\000\000\094\000\000\000\073\000"

let yytablesize = 289
let yytable = "\003\000\
\007\000\050\000\066\000\048\000\001\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\010\000\023\000\024\000\068\000\004\000\069\000\070\000\031\000\
\032\000\025\000\026\000\042\000\027\000\028\000\004\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\033\000\023\000\024\000\034\000\035\000\036\000\
\037\000\038\000\039\000\025\000\026\000\040\000\027\000\028\000\
\041\000\052\000\043\000\044\000\045\000\046\000\047\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\084\000\079\000\080\000\081\000\
\082\000\083\000\085\000\086\000\087\000\088\000\089\000\090\000\
\091\000\002\000\004\000\009\000\011\000\049\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\000\000\099\000\
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
\000\000\000\000\051\000\067\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000"

let yycheck = "\001\001\
\000\000\000\000\000\000\001\001\001\000\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\001\001\015\001\016\001\028\001\032\001\030\001\031\001\029\001\
\029\001\023\001\024\001\028\001\026\001\027\001\032\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\029\001\015\001\016\001\029\001\029\001\029\001\
\029\001\029\001\029\001\023\001\024\001\029\001\026\001\027\001\
\029\001\002\001\029\001\029\001\029\001\029\001\029\001\002\001\
\002\001\002\001\002\001\002\001\002\001\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\029\001\029\001\029\001\029\001\
\029\001\029\001\029\001\029\001\002\001\029\001\029\001\028\001\
\028\001\031\001\002\001\002\001\002\001\002\001\002\001\002\001\
\002\001\000\000\000\000\003\000\007\000\029\000\255\255\029\001\
\029\001\029\001\029\001\029\001\029\001\029\001\255\255\028\001\
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
\032\001"

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
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                      ( I_const (`L_Reg _2, `L_Str _4) )
# 365 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                     ( I_const (`L_Reg _2, `L_Id _4) )
# 373 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                    ( I_mov (`L_Reg _2, `L_Reg _4) )
# 381 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "parser.mly"
                              ( I_add (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 390 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                              ( I_sub (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 399 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
                              ( I_mul (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 408 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                              ( I_div (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 417 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                             ( I_eq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 426 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                             ( I_lt (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 435 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 47 "parser.mly"
                              ( I_leq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 444 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
                       ( I_is_int (`L_Reg _2, `L_Reg _4) )
# 452 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
                       ( I_is_str (`L_Reg _2, `L_Reg _4) )
# 460 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 51 "parser.mly"
          ( I_jmp _2 )
# 467 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
                        ( I_if_zero (`L_Reg _2, _4) )
# 475 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                               ( I_call (`L_Reg _2, _4, _6) )
# 484 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
          ( I_ret (`L_Reg _2) )
# 491 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                       ( I_int_sym (`L_Reg _2, _4) )
# 499 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
                  ( I_assert_zero (`L_Reg _2) )
# 506 "parser.ml"
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
