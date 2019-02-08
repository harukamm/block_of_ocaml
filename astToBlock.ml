open Parsetree
open Asttypes
open Longident

exception NotImplemented of string

let pp = Format.fprintf
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html#TYPEexpression *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Asttypes.html#TYPErec_flag *)
(* Reference: https://github.com/davidlazar/xml_of_ocaml *)

type arg_t = Argument of Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern

(* miscellaneous printers *)
let dom_rec_flag = function
  | Nonrecursive -> raise (NotImplemented "Nonrecursive")
  | Recursive -> raise (NotImplemented "Recursive")

let dom_direction_flag = function
  | Upto -> raise (NotImplemented "Upto")
  | Downto -> raise (NotImplemented "Downto")

let dom_mutable_flag = function
  | Immutable -> raise (NotImplemented "Immutable")
  | Mutable -> raise (NotImplemented "Mutable")

let dom_private_flag = function
  | Private -> raise (NotImplemented "Private")
  | Public -> raise (NotImplemented "Public")

let dom_virtual_flag = function
  | Virtual -> raise (NotImplemented "Virtual")
  | Concrete -> raise (NotImplemented "Concrete")

let dom_override_flag = function
  | Override -> raise (NotImplemented "Override")
  | Fresh -> raise (NotImplemented "Fresh")

let dom_strings () = raise (NotImplemented "dom_strings")

(* The type of identifiers (including path like Foo(X).Bar.y) *)
let rec dom_ident = function
  |	Lident (name) -> dom_id_block name
    (* i . i *) (** Access in module *)
  | Ldot (ident, name) -> raise (NotImplemented "LDot")
    (* i i *) (** Application *)
  | Lapply (ident1, ident2) -> raise (NotImplemented "Lapply")

and dom_construct = function
  | ({txt=Lident (ctr_name)}, None) ->
    if ctr_name = "true" then
      dom_bool_block true
    else if ctr_name = "false" then
      dom_bool_block false
    else if ctr_name = "[]" then
      dom_list_block []
    else
      raise (NotImplemented ("constructor: " ^ ctr_name))
  | ({txt=Lident ("::")}, Some {pexp_desc=Pexp_tuple lst}) -> dom_list_block lst
  | ({txt=Lident ("::")}, _) -> assert false
  | _ -> raise (NotImplemented "constructor")

and dom_constant = function
  | Pconst_integer (int_str, None) ->
    dom_int_block (int_of_string int_str)
  | Pconst_integer (int_str, Some suffx) ->
    let int_literal = int_str ^ (String.make 1 suffx) in
    let msg = "integer: " ^ int_literal in
    raise (NotImplemented msg)
  | Pconst_float (float_str, None) ->
    dom_float_block (float_of_string float_str)
  | Pconst_float (float_str, Some suffx) ->
    let float_literal = float_str ^ (String.make 1 suffx) in
    let msg = "float: " ^ float_literal in
    raise (NotImplemented msg)
  | Pconst_char c -> raise (NotImplemented "char literal")
  | Pconst_string (str, _) -> dom_string_block str

(* The type of patterns *)
and dom_patt patt = match patt with
  | Ppat_any -> raise (NotImplemented "Any")
  | Ppat_var (loc) -> raise (NotImplemented ("tvar" ^ loc.txt))
  | Ppat_tuple (patt_list) -> raise (NotImplemented "tuplepatt")
  | Ppat_record _ -> raise (NotImplemented "patt-record")
  | Ppat_type _ -> raise (NotImplemented "patt-type")
  | _ -> raise (NotImplemented "patt")

(* The type of expressions *)
and dom_expr expr = match expr.pexp_desc with
  | Pexp_ident loc -> dom_ident loc.txt
  | Pexp_let (rec_flag, [binding], expr) -> dom_let_block rec_flag binding (Some expr)
  | Pexp_let _ -> raise (NotImplemented "pexp_let")
  | Pexp_function _ -> raise (NotImplemented "function")
  | Pexp_fun (label, def, pat, expr) -> dom_fun_block label def pat expr
  | Pexp_apply (exp1, []) -> assert false
  | Pexp_apply (exp1, exp2 :: rest) ->
    if List.exists (fun (label, _) -> label <> Nolabel) (exp2 :: rest) then
      raise (NotImplemented "argument with label")
    else
      dom_app_lst_block exp1 (snd exp2) (List.map snd rest)
  | Pexp_match _ -> raise (NotImplemented "match")
  | Pexp_try _ -> raise (NotImplemented "try")
  | Pexp_tuple (x :: y :: rest) ->
    if List.length rest = 0 then
      dom_tuple_block x y
    else
      raise (NotImplemented "n-tuple (n < 2)")
  | Pexp_tuple _ -> assert false
  | Pexp_construct (ctr, opt) -> dom_construct (ctr, opt)
  | Pexp_variant _ -> raise (NotImplemented "variant")
  | Pexp_record _ -> raise (NotImplemented "Pexp_record")
  | Pexp_ifthenelse (cond, e1, Some e2) -> dom_ifthenelse_block cond e1 e2
  | Pexp_ifthenelse (cond, e1, None) -> raise (NotImplemented "if-without-else")
  | Pexp_constant constant -> dom_constant constant
  | Pexp_field _ -> raise (NotImplemented "field")
  | Pexp_setfield _ -> raise (NotImplemented "set field")
  | Pexp_array _ -> raise (NotImplemented "array")
  | Pexp_sequence _ -> raise (NotImplemented "sequence ';'")
  | Pexp_while _ -> raise (NotImplemented "while")
  | Pexp_for _ -> raise (NotImplemented "for")
  | Pexp_constraint _ -> raise (NotImplemented "constrait")
  | Pexp_coerce _ -> raise (NotImplemented "coerce")
  | Pexp_send _ -> raise (NotImplemented "send")
  | Pexp_new _ -> raise (NotImplemented "new")
  | Pexp_setinstvar _ -> raise (NotImplemented "setinstvar")
  | Pexp_override _ -> raise (NotImplemented "override")
  | Pexp_letmodule _ -> raise (NotImplemented "letmodule")
  | Pexp_letexception _ -> raise (NotImplemented "letexception")
  | Pexp_assert _ -> raise (NotImplemented "assert")
  | Pexp_lazy _ -> raise (NotImplemented "lazy")
  | Pexp_poly _ -> raise (NotImplemented "poly")
  | Pexp_object _ -> raise (NotImplemented "object")
  | Pexp_newtype _ -> raise (NotImplemented "newtype")
  | Pexp_pack _ -> raise (NotImplemented "pack")
  | Pexp_open _ -> raise (NotImplemented "open")
  | Pexp_extension _ -> raise (NotImplemented "extension")
  | Pexp_unreachable -> raise (NotImplemented ".")

(* The type of structure items *)
and dom_struct_item item = match item.pstr_desc with
  | Pstr_eval (expression, _) -> dom_expr expression
  | Pstr_value (rec_flag, [binding]) -> dom_let_block rec_flag binding None
  | Pstr_value _ -> raise (NotImplemented "Pstr_value")
  | Pstr_primitive (value_description) -> raise (NotImplemented "Pstr_primitive")
  | Pstr_type (rec_flag, type_declarations) -> raise (NotImplemented "Pstr_type")
  | Pstr_typext (type_extension) -> raise (NotImplemented "Pstr_typext")
  | Pstr_exception (extension_constructor) -> raise (NotImplemented "exception")
  | Pstr_module (module_binding) -> raise (NotImplemented "Pstr_module")
  | Pstr_recmodule (module_bindings) -> raise (NotImplemented "Pstr_recmodule")
  | Pstr_modtype (module_type_declaration) -> raise (NotImplemented "Pstr_modtype")
  | Pstr_open (open_description) -> raise (NotImplemented "Pstr_modtype")
  | Pstr_class (class_declarations) -> raise (NotImplemented "Pstr_class")
  | Pstr_class_type (class_type_declarations) -> raise (NotImplemented "Pstr_class_type")
  | Pstr_include (include_declaration) -> raise (NotImplemented "Pstr_include")
  | Pstr_attribute (attribute) -> raise (NotImplemented "Pstr_attribute")
  | Pstr_extension (extension, attributes) -> raise (NotImplemented "Pstr_extension")

and dom_struct_items = function
  | [] -> Xml.createNilDom()
  | x :: xs ->
    let dom = dom_struct_item x in
    if xs = []
    then dom
    else
      let children = dom_struct_items xs in
      append_next dom children

and flatten_arguments exp = match exp.pexp_desc with
  | Pexp_fun (label, def, pat, exp') ->
    let arg = Argument (label, def, pat) in
    let (args, rest) = flatten_arguments exp' in
    (arg :: args, rest)
  | _ -> ([], exp)

and dom_int_block n = dom_block "int_typed" [dom_field "INT" (string_of_int n)]

and dom_float_block n = dom_block "float_typed" [dom_field "Float" (string_of_float n)]

and dom_string_block str = dom_block "string_typed" [dom_field "STRING" str]

and dom_id_block id = dom_block "variables_get_typed" [dom_var_field "VAR" false id]

and dom_bool_block isTrue =
  let upper_value = if isTrue then "TRUE" else "FALSE" in
  dom_block "logic_boolean_typed" [dom_field "BOOL" upper_value]

and dom_ifthenelse_block cond e1 e2 =
  let domCond = dom_expr cond in
  let domExp1 = dom_expr e1 in
  let domExp2 = dom_expr e2 in
  let dom = dom_block "logic_ternary_typed" [] in
  let dom = append_value dom "IF" domCond in
  let dom = append_value dom "THEN" domExp1 in
  let dom = append_value dom "ELSE" domExp2 in
  dom

and dom_tuple_block e1 e2 =
  let domExp1 = dom_expr e1 in
  let domExp2 = dom_expr e2 in
  let dom = dom_block "pair_create_typed" [] in
  let dom = append_value dom "FIRST" domExp1 in
  let dom = append_value dom "SECOND" domExp2 in
  dom

and dom_pattern pat =
  match pat.ppat_desc with
  | Ppat_var var -> dom_var_field "VAR" true var.txt
  | _ -> raise (NotImplemented "Unsupported pattern")

and dom_let_block rec_flag binding opt_exp2 =
  let is_rec = match rec_flag with
    | Recursive -> true
    | Nonrecursive -> false in
  let is_statement = opt_exp2 = None in
  match (binding, opt_exp2) with
  | ({pvb_pat=patt; pvb_expr=exp1}, _) ->
     let field = dom_pattern patt in
     let (args, exp1) = flatten_arguments exp1 in
     let (mutation, arg_fields) = dom_arguments args in
     let dom = dom_block "let_typed" (mutation :: field :: arg_fields) in
     let dom = Xml.setAttribute dom ("rec",
                                     if is_rec then "true" else "false") in
     let dom = Xml.setAttribute dom ("statement",
                                     if is_statement then "true" else "false") in
     let domExp1 = dom_expr exp1 in
     let dom = append_value dom "EXP1" domExp1 in
     let dom =
       match opt_exp2 with
         | None -> dom
         | Some exp2 ->
           let domExp2 = dom_expr exp2 in
           append_value dom "EXP2" domExp2
     in
     dom

and dom_arguments args =
  let names = List.map (function
      | Argument(_, _, {ppat_desc=Ppat_var var}) -> var.txt
      | _ -> raise (NotImplemented "Unsupported pattern")) args in
  let mutation = arguments_mutation names in
  let rec h i names = match names with
    | [] -> []
    | x :: xs ->
      let field = dom_var_field ("ARG" ^ (string_of_int i)) true x in
      field :: (h (i + 1) xs)
  in
  let arg_fields = h 0 names in
  (mutation, arg_fields)

and dom_label label =
  match label with
  | Nolabel -> raise (NotImplemented "no label")
  | Labelled l -> (dom_field "VAR" l)
  | Optional l -> raise (NotImplemented "optional label")

and dom_fun_block label def pat expr =
  (* Note label and def are ignored *)
  let domExpr = dom_expr expr in
  let dom = dom_block "lambda_typed" [] in
  let dom = Xml.appendChild dom (dom_pattern pat) in
  let dom = append_value dom "RETURN" domExpr in
  match def with
  | Some _ -> raise (NotImplemented "fun def")
  | None -> dom

and dom_binary_op block_type op_type op_kind exp1 exp2 =
  let domExp1 = dom_expr exp1 in
  let domExp2 = dom_expr exp2 in
  let dom = dom_block block_type [] in
  let dom = Xml.appendChild dom (dom_field op_type op_kind) in
  let dom = append_value dom "A" domExp1 in
  let dom = append_value dom "B" domExp2 in
  dom

and dom_int_binary_op op exp1 exp2 =
  dom_binary_op "int_arithmetic_typed" "OP_INT" (op ^ "_INT") exp1 exp2

and dom_float_binary_op op exp1 exp2 =
  dom_binary_op "float_arithmetic_typed" "OP_FLOAT" (op ^ "_FLOAT") exp1 exp2

and dom_cmp_binary_op op exp1 exp2 =
  dom_binary_op "logic_compare_typed" "OP" op exp1 exp2

and dom_string_concat exp1 exp2 =
  let domExp1 = dom_expr exp1 in
  let domExp2 = dom_expr exp2 in
  let dom = dom_block "concat_string_typed" [] in
  let dom = append_value dom "A" domExp1 in
  let dom = append_value dom "B" domExp2 in
  dom

and dom_maybe_binary_op exp1 exp_list =
  match exp_list with
  | exp_lhs :: [exp_rhs] -> (
    match exp1.pexp_desc with
    | Pexp_ident loc -> (
      match loc.txt with
      | Lident "+" -> Some (dom_int_binary_op "ADD" exp_lhs exp_rhs)
      | Lident "-" -> Some (dom_int_binary_op "MINUS" exp_lhs exp_rhs)
      | Lident "*" -> Some (dom_int_binary_op "MULTIPLY" exp_lhs exp_rhs)
      | Lident "/" -> Some (dom_int_binary_op "DIVIDE" exp_lhs exp_rhs)
      | Lident "+." -> Some (dom_float_binary_op "ADD" exp_lhs exp_rhs)
      | Lident "-." -> Some (dom_float_binary_op "MINUS" exp_lhs exp_rhs)
      | Lident "*." -> Some (dom_float_binary_op "MULTIPLY" exp_lhs exp_rhs)
      | Lident "/." -> Some (dom_float_binary_op "DIVIDE" exp_lhs exp_rhs)
      | Lident "=" -> Some (dom_cmp_binary_op "EQ" exp_lhs exp_rhs)
      | Lident "!=" -> Some (dom_cmp_binary_op "NEQ" exp_lhs exp_rhs)
      | Lident "<" -> Some (dom_cmp_binary_op "LT" exp_lhs exp_rhs)
      | Lident "<=" -> Some (dom_cmp_binary_op "LTE" exp_lhs exp_rhs)
      | Lident ">" -> Some (dom_cmp_binary_op "GT" exp_lhs exp_rhs)
      | Lident ">=" -> Some (dom_cmp_binary_op "GTE" exp_lhs exp_rhs)
      | Lident "^" -> Some (dom_string_concat exp_lhs exp_rhs)
      | _ -> None
    )
    | _ -> None
  )
  | _ -> None

and dom_app_lst_block exp1 exp2 exp2_rest =
  match dom_maybe_binary_op exp1 (exp2 :: exp2_rest) with
  | Some dom -> dom
  | None ->
    match exp1.pexp_desc with
    | Pexp_ident ({txt=Lident name}) ->
      begin
        if exp2_rest = [] then
          if name = "fst" then dom_builtint_fst_app exp2
          else if name = "snd" then dom_builtint_snd_app exp2
          else if name = "string_of_int" then dom_builtint_string_of_int_app exp2
          else dom_varapp_lst_block name [exp2]
        else
          dom_varapp_lst_block name (exp2 :: exp2_rest)
      end
    | _ ->
     let left = dom_app_block exp1 exp2 in
     List.fold_left (fun dom exp -> dom_app_block' dom (dom_expr exp)) left exp2_rest

and dom_varapp_lst_block id lst =
  let field = dom_var_field "VAR" false id in
  let mutation = params_mutation (List.length lst) in
  let dom = dom_block "function_app_typed" [field; mutation] in
  let rec h dom' i exprs = match exprs with
    | [] -> dom'
    | exp :: rest ->
      let name = "PARAM" ^ (string_of_int i) in
      let dom'' = append_value dom' name (dom_expr exp) in
      h dom'' (i + 1) rest
  in
  h dom 0 lst

and dom_app_block' dom_exp1 dom_exp2 =
  let dom = dom_block "lambda_app_typed" [] in
  let dom = append_value dom "FUN" dom_exp1 in
  let dom = append_value dom "ARG" dom_exp2 in
  dom

and dom_app_block expr1 expr2 =
  let domExp1 = dom_expr expr1 in
  let domExp2 = dom_expr expr2 in
  dom_app_block' domExp1 domExp2

and params_mutation n =
  Xml.createDom "mutation" [("params", string_of_int n)] []

and arguments_mutation names =
  let children = List.map (fun name ->
      Xml.createDom "item" [] [Xml.createTextDom name]) names in
  Xml.createDom "mutation" [] children

and dom_list_block exprs =
  let rec h dom es = match es with
    | [] -> dom
    | expr :: rest ->
      let nth = (List.length exprs) - (List.length es) in
      let nstr = string_of_int nth in
      let dom' = append_value dom ("ADD" ^ nstr) (dom_expr expr) in
      h dom' rest
  in
  let dom = dom_block "lists_create_with_typed" [] in
  h dom exprs

and dom_builtint_fst_app exp =
  let domExp = dom_expr exp in
  let value = dom_block_value "FIRST" domExp in
  dom_block "pair_first_typed" [value]

and dom_builtint_snd_app exp =
  let domExp = dom_expr exp in
  let value = dom_block_value "SECOND" domExp in
  dom_block "pair_second_typed" [value]

and dom_builtint_string_of_int_app exp =
  let domExp = dom_expr exp in
  let value = dom_block_value "PARAM" domExp in
  dom_block "string_of_int_typed" [value]

and dom_block typeName children =
  Xml.createDom "block" [("type", typeName)] children

and dom_block_value name child =
  Xml.createDom "value" [("name", name)] [child]

and dom_next_block child =
  Xml.createDom "next" [] [child]

and append_value xml name child =
  Xml.appendChild xml (dom_block_value name child)

and append_next xml child =
  Xml.appendChild xml (dom_next_block child)

and dom_field name text =
  Xml.createDom "field" [("name", name)] [Xml.createTextDom text]

and dom_var_field name isValue text =
  let field = dom_field name text in
  Xml.setAttribute field ("isvalue", if isValue then "true" else "false")
