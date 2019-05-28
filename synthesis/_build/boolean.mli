type bexpr =
    EFalse
  | ETrue
  | EVar of string
  | EAnd of bexpr * bexpr
  | EOr of bexpr * bexpr
  | ENot of bexpr
  | EForall of string * bexpr
  | EExists of string * bexpr

type asst = (string * bool) list

val eval : asst -> bexpr -> bool
val free_vars : bexpr -> string list
val sat : bexpr -> asst option
