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
  | INT of (int)
  | REG of (int)
  | STR of (string)
  | ID of (string)
  | FN of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Instr
# 39 "parser.ml"
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
    0|]

let yytransl_block = [|
  282 (* INT *);
  283 (* REG *);
  284 (* STR *);
  285 (* ID *);
  286 (* FN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\001\000\001\000\002\000\003\000\002\000\
\002\000\003\000\003\000\002\000\004\000\004\000\004\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\004\000\
\004\000\004\000\002\000\004\000\004\000\004\000\002\000\006\000\
\006\000\006\000\006\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\038\000\001\000\000\000\005\000\
\003\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\036\000\037\000\012\000\000\000\009\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\010\000\013\000\014\000\015\000\016\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\025\000\026\000\028\000\029\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\032\000\033\000\034\000\035\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\035\000\036\000"

let yysindex = "\004\000\
\255\254\000\000\255\254\052\255\000\000\000\000\025\255\000\000\
\000\000\027\255\000\000\031\255\032\255\033\255\034\255\035\255\
\036\255\037\255\038\255\039\255\040\255\041\255\042\255\044\255\
\045\255\046\255\047\255\048\255\050\255\051\255\053\255\054\255\
\055\255\056\255\003\255\002\000\069\255\072\255\077\255\082\255\
\083\255\084\255\085\255\086\255\087\255\088\255\089\255\090\255\
\000\000\091\255\092\255\093\255\000\000\094\255\095\255\096\255\
\097\255\000\000\000\000\000\000\003\000\000\000\000\000\028\255\
\073\255\074\255\075\255\076\255\078\255\079\255\080\255\081\255\
\098\255\099\255\100\255\102\255\101\255\104\255\105\255\106\255\
\107\255\103\255\000\000\000\000\000\000\000\000\000\000\000\000\
\108\255\109\255\110\255\111\255\112\255\113\255\114\255\000\000\
\000\000\000\000\000\000\000\000\000\000\115\255\116\255\117\255\
\118\255\119\255\120\255\121\255\122\255\123\255\124\255\125\255\
\126\255\127\255\128\255\130\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\104\000\000\000\104\000\000\000\000\000\000\000\109\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\118\000\000\000\115\000\000\000\088\000"

let yytablesize = 287
let yytable = "\003\000\
\007\000\062\000\083\000\060\000\001\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\004\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\010\000\085\000\004\000\086\000\
\087\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\064\000\050\000\
\051\000\065\000\053\000\052\000\054\000\055\000\066\000\056\000\
\057\000\058\000\059\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\088\000\089\000\090\000\091\000\002\000\
\092\000\093\000\094\000\095\000\004\000\106\000\107\000\108\000\
\109\000\110\000\111\000\112\000\113\000\114\000\115\000\116\000\
\009\000\011\000\061\000\000\000\096\000\097\000\098\000\099\000\
\105\000\100\000\101\000\102\000\103\000\104\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\117\000\118\000\119\000\120\000\121\000\122\000\123\000\
\124\000\125\000\126\000\127\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\063\000\084\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000"

let yycheck = "\001\001\
\000\000\000\000\000\000\001\001\001\000\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\030\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\001\001\026\001\030\001\028\001\
\029\001\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
\027\001\027\001\027\001\027\001\027\001\026\001\002\001\027\001\
\027\001\002\001\027\001\029\001\027\001\027\001\002\001\027\001\
\027\001\027\001\027\001\002\001\002\001\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\002\001\002\001\002\001\002\001\
\002\001\002\001\002\001\027\001\027\001\027\001\027\001\000\000\
\027\001\027\001\027\001\027\001\000\000\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\002\001\002\001\002\001\002\001\
\003\000\007\000\035\000\255\255\027\001\027\001\027\001\026\001\
\026\001\029\001\027\001\027\001\027\001\027\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
\027\001\027\001\027\001\026\001\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\030\001"

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
# 14 "parser.mly"
       ( List.rev _1 )
# 272 "parser.ml"
               : (string * Instr.instr list) list))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
  ( [] )
# 278 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 18 "parser.mly"
           ( _2 )
# 285 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'progp) in
    Obj.repr(
# 19 "parser.mly"
        ( _1 )
# 292 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fn) in
    Obj.repr(
# 22 "parser.mly"
     ( [_1] )
# 299 "parser.ml"
               : 'progp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'progp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fn) in
    Obj.repr(
# 23 "parser.mly"
           ( _2::_1 )
# 307 "parser.ml"
               : 'progp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 26 "parser.mly"
                ( _1, List.rev _3 )
# 315 "parser.ml"
               : 'fn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 29 "parser.mly"
            ( [_1] )
# 322 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 30 "parser.mly"
            ( [_1] )
# 329 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instrs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 31 "parser.mly"
                   ( _2::_1 )
# 337 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instrs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 32 "parser.mly"
                   ( _2::_1 )
# 345 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 33 "parser.mly"
             ( _1 )
# 352 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 36 "parser.mly"
                      ( I_const (`L_Reg _2, `L_Int _4) )
# 360 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                      ( I_const (`L_Reg _2, `L_Str _4) )
# 368 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                     ( I_const (`L_Reg _2, `L_Id _4) )
# 376 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "parser.mly"
                    ( I_mov (`L_Reg _2, `L_Reg _4) )
# 384 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                              ( I_add (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 393 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "parser.mly"
                              ( I_sub (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 402 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                              ( I_mul (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 411 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
                              ( I_div (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 420 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                             ( I_eq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 429 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                             ( I_lt (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 438 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                              ( I_leq (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 447 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 47 "parser.mly"
                       ( I_is_int (`L_Reg _2, `L_Reg _4) )
# 455 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
                       ( I_is_str (`L_Reg _2, `L_Reg _4) )
# 463 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
                       ( I_is_tab (`L_Reg _2, `L_Reg _4) )
# 471 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser.mly"
          ( I_jmp _2 )
# 478 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 51 "parser.mly"
                        ( I_if_zero (`L_Reg _2, _4) )
# 486 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                       ( I_rd_glob (`L_Reg _2, `L_Id _4) )
# 494 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
                       ( I_wr_glob (`L_Id _2, `L_Reg _4) )
# 502 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
             ( I_mk_tab (`L_Reg _2) )
# 509 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
                                 ( I_rd_tab (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 518 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
                                 ( I_wr_tab (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 527 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
                                  ( I_has_tab (`L_Reg _2, `L_Reg _4, `L_Reg _6) )
# 536 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 58 "parser.mly"
                               ( I_call (`L_Reg _2, _4, _6) )
# 545 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
          ( I_ret (`L_Reg _2) )
# 552 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
           ( I_halt (`L_Reg _2) )
# 559 "parser.ml"
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
