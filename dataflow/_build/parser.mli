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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Instr.instr list
