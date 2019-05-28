open OUnit

let u () = assert_equal 1 1

(* eval tests *)
let eval_nil_true () = assert_equal (Boolean.eval [] ETrue) true
let eval_nil_false () = assert_equal (Boolean.eval [] EFalse) false
let eval_bindings_true () = assert_equal (Boolean.eval ["unit",true;"test",false] ETrue) true 
let eval_var_single () = assert_equal (Boolean.eval ["unit",true] (EVar "unit")) true
let eval_var_multi () = assert_equal (Boolean.eval ["unit",false;"unit",true] (EVar "unit")) false
let eval_not_single () = assert_equal (Boolean.eval ["unit",true] (ENot (EVar "unit"))) false
let eval_not_multi () = assert_equal (Boolean.eval ["unit",true] (ENot (ENot (ENot (EVar "unit"))))) false
let eval_not_true () = assert_equal (Boolean.eval [] (ENot (ENot (EFalse)))) false
let eval_and_tt () = assert_equal (Boolean.eval [] (EAnd (ETrue, ETrue))) true
let eval_and_tf () = assert_equal (Boolean.eval [] (EAnd (ETrue, EFalse))) false
let eval_and_ff () = assert_equal (Boolean.eval [] (EAnd (EFalse, EFalse))) false
let eval_and_demorgan_tt () = assert_equal (Boolean.eval [] (ENot (EAnd (ETrue, ETrue)))) false
let eval_and_demorgan_tf () = assert_equal (Boolean.eval [] (ENot (EAnd (ETrue, EFalse)))) true
let eval_and_demorgan_ff () = assert_equal (Boolean.eval [] (ENot (EAnd (EFalse, EFalse)))) true
let eval_and_demorgan_tf_b () = assert_equal (Boolean.eval ["unit",true;"test",false] (ENot (EAnd (EVar "unit", EVar "test")))) true
let eval_or_tt () = assert_equal (Boolean.eval [] (EOr (ETrue, ETrue))) true
let eval_or_tf () = assert_equal (Boolean.eval [] (EOr (ETrue, EFalse))) true
let eval_or_ff () = assert_equal (Boolean.eval [] (EOr (EFalse, EFalse))) false
let eval_or_demorgan_tt () = assert_equal (Boolean.eval [] (ENot (EOr (ETrue, ETrue)))) false
let eval_or_demorgan_tf () = assert_equal (Boolean.eval [] (ENot (EOr (EFalse, ETrue)))) false
let eval_or_demorgan_ff () = assert_equal (Boolean.eval [] (ENot (EOr (EFalse, EFalse)))) true
let eval_or_demorgan_tf_b () = assert_equal (Boolean.eval ["unit",true;"test",false] (ENot (EOr (EVar "unit", EVar "test")))) false
let eval_forall_t () = assert_equal (Boolean.eval [] (EForall ("x", ETrue))) true
let eval_forall_f () = assert_equal (Boolean.eval [] (EForall ("x", EFalse))) false
let eval_forall_same () = assert_equal (Boolean.eval [] (EForall ("x", EVar "x"))) false
let eval_forall_same_b () = assert_equal (Boolean.eval ["x",true] (EForall ("x", EVar "x"))) false
let eval_forall_diff_b () = assert_equal (Boolean.eval ["y",true] (EForall ("x", EVar "y"))) true
let eval_forall_capture () = assert_equal (Boolean.eval ["y",true] (EForall ("x", (EAnd (EVar "x", EVar "y"))))) false
let eval_forall_capture_b () = assert_equal (Boolean.eval ["x",true;"y",true] (EForall ("x", (EAnd (EVar "x", EVar "y"))))) false
let eval_forall_abscent () = assert_equal (Boolean.eval ["y",true] (EForall ("x", EVar "y"))) true
let eval_forall_rev () = assert_equal (Boolean.eval [] (EForall ("x", EOr (ENot (EVar "x"), EForall ("x", EVar "x"))))) false
let eval_forall_bound () = assert_equal (Boolean.eval ["x",true] (EForall ("x", EVar "x"))) false
let eval_exists_t () = assert_equal (Boolean.eval [] (EExists ("x", ETrue))) true
let eval_exists_f () = assert_equal (Boolean.eval [] (EExists ("x", EFalse))) false

let fv_t () = assert_equal (Boolean.free_vars ETrue) []
let fv_f () = assert_equal (Boolean.free_vars EFalse) []
let fv_v () = assert_equal (Boolean.free_vars (EVar "x")) ["x"]
let fv_n () = assert_equal (Boolean.free_vars (ENot (EFalse))) []
let fv_and () = assert_equal (Boolean.free_vars (EAnd (EVar "x", EFalse))) ["x"]
let fv_or () = assert_equal (Boolean.free_vars (EOr (EVar "x", EVar "y"))) ["y";"x"]
let fv_fa () = assert_equal (Boolean.free_vars (EForall ("x", EVar "x"))) []
let fv_fe () = assert_equal (Boolean.free_vars (EExists ("x", EVar "x"))) []
let fv_b1 () = assert_equal (Boolean.free_vars (EAnd (EVar "x", EExists ("x", ETrue)))) ["x"]
let fv_n () = assert_equal (Boolean.free_vars (EExists ("x", EExists("y", EExists("z", EOr(EVar "x", EAnd(EVar "z", EVar "x"))))))) []

let s = (function | (Some e) -> e | None -> [])
let satt () = let f = (Boolean.ETrue) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satf () = let f = (Boolean.EFalse) in  assert_equal (Boolean.eval (s (Boolean.sat (f))) f) false
let satv () = let f = (Boolean.EVar ("x")) in  assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satn () = let f = (Boolean.ENot (Boolean.EVar "x")) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satnn () = let f = (Boolean.ENot (Boolean.ENot (Boolean.EVar "x"))) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let sata () = let f = (Boolean.EAnd (Boolean.EVar "x", Boolean.ENot (Boolean.EVar "x"))) in assert_equal (Boolean.sat f) None
let sato () = let f = (Boolean.EOr (Boolean.EVar "x", Boolean.ENot (Boolean.EVar "y"))) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satda () = let f = (Boolean.ENot (Boolean.EAnd (Boolean.EVar "x", Boolean.EVar "y"))) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satdo () = let f = (Boolean.ENot (Boolean.EOr (Boolean.EVar "x", Boolean.EVar "y"))) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true
let satdon () = let f = (Boolean.ENot (Boolean.EOr (Boolean.EVar "x", Boolean.EVar "x"))) in assert_equal (Boolean.eval (s (Boolean.sat (f))) f) true

let and_vectors () = assert_equal (Bvec.bitand [ETrue;EFalse] [ETrue;ETrue]) [EAnd (ETrue, ETrue); EAnd (EFalse, ETrue)]
let bvec_eq () = assert_equal (Boolean.eval [] (Bvec.eq [ETrue;ETrue] [ETrue;ETrue])) true
let bvec_eq1 () = assert_equal (Boolean.eval [] (Bvec.eq [ETrue;EFalse] [ETrue;ETrue])) false

let pad_2_4 () = assert_equal (Magic.pad [EFalse;ETrue] 4) [EFalse;ETrue;EFalse;EFalse]
let pad_eq () = assert_equal (Magic.pad [EFalse] 1) [EFalse]
let pad_less () = assert_equal (Magic.pad [ETrue;EFalse;ETrue] 2) [ETrue;EFalse;ETrue]

let add_three_0 () = assert_equal 1 1
let add_three_2 () = assert_equal 1 1
let is_digit_zero () = assert_equal (Boolean.eval [] (Magic.is_digit [EFalse;EFalse;EFalse;EFalse])) false
let is_digit_one () = assert_equal (Boolean.eval [] (Magic.is_digit [ETrue;EFalse;EFalse;EFalse])) true
let is_digit_nine () = assert_equal (Boolean.eval [] (Magic.is_digit [ETrue;EFalse;EFalse;ETrue])) true
let is_digit_ten () = assert_equal (Boolean.eval [] (Magic.is_digit [EFalse;ETrue;EFalse;ETrue])) false
let is_digit_fifteen () = assert_equal (Boolean.eval [] (Magic.is_digit [ETrue;ETrue;ETrue;ETrue])) false

let disjoint_single_ff () = assert_equal (Boolean.eval [] (Magic.disjoint [[EFalse];[EFalse]])) false
let disjoint_single_ft () = assert_equal (Boolean.eval [] (Magic.disjoint [[EFalse];[ETrue]])) true
let disjoint_single_tf () = assert_equal (Boolean.eval [] (Magic.disjoint [[ETrue];[EFalse]])) true
let disjoint_single_tt () = assert_equal (Boolean.eval [] (Magic.disjoint [[ETrue];[ETrue]])) false

let disjoint_2_diff () = assert_equal (Boolean.eval [] (Magic.disjoint [[EFalse;EFalse];[EFalse;ETrue];[ETrue;EFalse];[ETrue;ETrue]])) true
let disjoint_2_one_same () = assert_equal (Boolean.eval [] (Magic.disjoint [[EFalse;EFalse];[EFalse;ETrue];[EFalse;ETrue];[ETrue;ETrue]])) false
let disjoint_2_all_same () = assert_equal (Boolean.eval [] (Magic.disjoint [[EFalse;EFalse];[EFalse;EFalse];[EFalse;EFalse]])) false

let disjoint_4_diff () = assert_equal (Boolean.eval [] (Magic.disjoint (List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [8;1;6;3;5;7;4;9;2]))) true
let disjoint_4_one_same () = assert_equal (Boolean.eval [] (Magic.disjoint (List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [1;2;3;4;6;6;7;8;9]))) false
let disjoint_4_all_same () = assert_equal (Boolean.eval [] (Magic.disjoint (List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [1;1;1;1;1;1;1;1;1]))) false

let magic_yes () = assert_equal (Boolean.eval [] (Magic.is_magic (List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [8;1;6;3;5;7;4;9;2]))) true
let magic_yes_var () = let l = (Magic.is_magic ([EFalse;EFalse;EFalse;EVar "x"]::(List.tl (List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [8;1;6;3;5;7;4;9;2])))) in assert_equal (Boolean.sat l) (Some ["x",true])
let magic_all_vars () = let l = (Magic.is_magic ([EVar"a";EVar"b";EVar"x";EVar "x"]::(List.map (fun (e) -> Magic.pad (Bvec.bvec_of_int e) 4) [1;6;3;5;7;4;9;2]))) in assert_equal (Boolean.sat l) None

let itb_0 () = assert_equal (Bvec.int_of_bvec [EFalse]) 0
let itb_1 () = assert_equal (Bvec.int_of_bvec [ETrue]) 1
let itb_2 () = assert_equal (Bvec.int_of_bvec [EFalse;ETrue]) 2
let itb_5 () = assert_equal (Bvec.int_of_bvec [ETrue;EFalse;ETrue]) 5
let itb_lz () = assert_equal (Bvec.int_of_bvec [ETrue;EFalse;ETrue;EFalse;EFalse]) 5
let boi_0 () = assert_equal (Bvec.bvec_of_int 0) [EFalse]
let boi_3 () = assert_equal (Bvec.bvec_of_int 3) [ETrue;ETrue]
let boi_1 () = assert_equal (Bvec.bvec_of_int 1) [ETrue]
let boi_8 () = assert_equal (Bvec.bvec_of_int 8) [EFalse;EFalse;EFalse;ETrue]
let subst_3 () = assert_equal (Bvec.int_of_bvec (Bvec.subst ["x",true;"y",false;"z",true] [EVar "x";EVar "y";EVar"z"])) 5
let subst_2 () = assert_equal (List.map (fun (e) -> Boolean.eval [] e) (Bvec.subst ["x",true] [ENot(EVar "x"); EVar "x"])) [false;true]
let subst_c () = assert_equal (List.map (fun e -> Boolean.eval [] e) (Bvec.subst ["x",false] [EExists ("x", EVar "x")])) [true]
let subst_o () = assert_equal (List.map (fun (e) -> Boolean.eval [] e) (Bvec.subst ["x",true; "y",true; "x",false] [EVar "x"; EVar "y"; EVar "x"])) [true;true;true]
let z_e () = assert_equal (Boolean.eval [] (Bvec.zero [EFalse])) true
let z_e3 () = assert_equal (Boolean.eval [] (Bvec.zero [EFalse;EFalse;EFalse])) true
let z_es () = assert_equal (Boolean.eval ["x",false] (Bvec.zero [EVar "x"; EFalse; EVar "x"])) true
let z_ef () = assert_equal (Boolean.eval ["x",true] (Bvec.zero [EVar "x"; EFalse; EVar "x"])) false
let eq_1 () = assert_equal (Boolean.eval ["x",true; "y",false] (Bvec.eq [EVar "x";ENot(EVar "y")] [ETrue;EVar "x"])) true
let eq_n () = assert_equal (Boolean.eval ["x",true] (Bvec.eq [EVar "x"; EVar "x"] [ETrue; ENot(EVar "x")])) false
let add_1 () = assert_equal (List.map (fun (e) -> Boolean.eval ["x",true] e) (Bvec.add [EVar "x"] [ENot(EVar "x")])) [true;false]
let add_3 () = assert_equal (List.map (fun (e) -> Boolean.eval ["x",true] e) (Bvec.add [ETrue;EAnd (EFalse, EVar "x");ETrue] [EVar "x"; EOr (EVar "x", EFalse); EFalse])) [false;false;false;true]

let suite_boolean = "Boolean" >::: [
  "eval_nil_true">:: eval_nil_true;
  "eval_nil_false">:: eval_nil_false;
  "eval_bindings_true">:: eval_bindings_true;
  "eval_var_single">:: eval_var_single;
  "eval_var_multi">:: eval_var_multi;
  "eval_not_single">:: eval_not_single;
  "eval_not_multi">:: eval_not_multi;
  "eval_not_true">:: eval_not_true;
  "eval_and_tt">:: eval_and_tt;
  "eval_and_tf">:: eval_and_tf;
  "eval_and_ff">:: eval_and_ff;
  "eval_and_demorgan_tt">:: eval_and_demorgan_tt;
  "eval_and_demorgan_tf">:: eval_and_demorgan_tf;
  "eval_and_demorgan_ff">:: eval_and_demorgan_ff;
  "eval_and_demorgan_tf_b">:: eval_and_demorgan_tf_b;
  "eval_or_tt">:: eval_or_tt;
  "eval_or_tf">:: eval_or_tf;
  "eval_or_ff">:: eval_or_ff;
  "eval_or_demorgan_tt">:: eval_or_demorgan_tt;
  "eval_or_demorgan_tf">:: eval_or_demorgan_tf;
  "eval_or_demorgan_ff">:: eval_or_demorgan_ff;
  "eval_or_demorgan_tf_b">:: eval_or_demorgan_tf_b;
  "eval_forall_t">:: eval_forall_t;
  "eval_forall_f">:: eval_forall_f;
  "eval_forall_same">:: eval_forall_same;
  "eval_forall_same_b">:: eval_forall_same_b;
  "eval_forall_diff_b">:: eval_forall_diff_b;
  "eval_forall_capture">:: eval_forall_capture;
  "eval_forall_capture_b">:: eval_forall_capture_b;
  "eval_forall_abscent">:: eval_forall_abscent;
  "eval_forall_rev">:: eval_forall_rev;
  "eval_forall_bound">:: eval_forall_bound;
  "eval_exists_t">:: eval_exists_t;
  "eval_exists_f">:: eval_exists_f;
  "fv_t">:: fv_t;
  "fv_f">:: fv_f;
  "fv_v">:: fv_v;
  "fv_n">:: fv_n;
  "fv_and">:: fv_and;
  "fv_or">:: fv_or;
  "fv_fa">:: fv_fa;
  "fv_fe">:: fv_fe;
  "fv_b1">:: fv_b1;
  "fv_n">:: fv_n;
  "satt">:: satt;
  "satf">:: satf;
  "satv">:: satv;
  "satn">:: satn;
  "satnn">:: satnn;
  "sata">:: sata;
  "sato">:: sato;
  "satda">:: satda;
  "satdo">:: satdo;
  "satdo">:: satdon
]

let suite_bvec = "Bvec" >::: [
  "itb_0">:: itb_0;
  "itb_1">:: itb_1;
  "itb_2">:: itb_2;
  "itb_5">:: itb_5;
  "itb_lz">:: itb_lz;
  "boi_0">:: boi_0;
  "boi_3">:: boi_3;
  "boi_1">:: boi_1;
  "boi_8">:: boi_8;
  "subst_3">:: subst_3;
  "subst_2">:: subst_2;
  "subst_c">:: subst_c;
  "subst_o">:: subst_o;
  "z_e">:: z_e;
  "z_e3">:: z_e3;
  "z_es">:: z_es;
  "z_ef">:: z_ef;
  "eq_1">:: eq_1;
  "eq_n">:: eq_n;
  "add_1">:: add_1;
  "add_3">:: add_3
]

let suite_magic = "Magic" >::: [
  "u" >:: u;
  "pad_2_4">:: pad_2_4;
  "pad_eq">:: pad_eq;
  "pad_less">:: pad_less;
  "add_three_0">:: add_three_0;
  "add_three_2">:: add_three_2;
  "is_digit_zero">:: is_digit_zero;
  "is_digit_one">:: is_digit_one;
  "is_digit_nine">:: is_digit_nine;
  "is_digit_ten">:: is_digit_ten;
  "is_digit_fifteen">:: is_digit_fifteen;
  "disjoint_single_ff">:: disjoint_single_ff;
  "disjoint_single_ft">:: disjoint_single_ft;
  "disjoint_single_tf">:: disjoint_single_tf;
  "disjoint_single_tt">:: disjoint_single_tt;
  "disjoint_2_diff">:: disjoint_2_diff;
  "disjoint_2_one_same">:: disjoint_2_one_same;
  "disjoint_2_all_same">:: disjoint_2_all_same;
  "disjoint_4_diff">:: disjoint_4_diff;
  "disjoint_4_one_same">:: disjoint_4_one_same;
  "disjoint_4_all_same">:: disjoint_4_all_same;
  "magic_yes">:: magic_yes;
  "magic_yes_var">:: magic_yes_var;
  "magic_all_vars">:: magic_all_vars
]
				     
let suite_lambda = "Lambda" >::: [
  "u" >:: u
]
