open Parsetree
open Asttypes
open Longident

exception NotImplemented of string

let pp = Format.fprintf
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html#TYPEexpression *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Asttypes.html#TYPErec_flag *)
(* Reference: https://github.com/davidlazar/xml_of_ocaml *)

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
  | Pconst_string _ -> raise (NotImplemented "string literal")

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
  | Pexp_let (rec_flag, [binding], expr) -> dom_let_block rec_flag binding expr
  | Pexp_let _ -> raise (NotImplemented "pexp_let")
  | Pexp_function _ -> raise (NotImplemented "function")
  | Pexp_fun _ -> raise (NotImplemented "fun")
  | Pexp_apply (exp1, []) -> assert false
  | Pexp_apply (exp1, exp2 :: rest) ->
    if List.exists (fun (label, _) -> label <> Nolabel) (exp2 :: rest) then
      raise (NotImplemented "argument with label")
    else
      dom_app_lst_block exp1 (snd exp2) (List.map snd rest)
  | Pexp_match _ -> raise (NotImplemented "match")
  | Pexp_try _ -> raise (NotImplemented "try")
  | Pexp_tuple _ -> raise (NotImplemented "tuple")
  | Pexp_construct (ctr, opt) -> dom_construct (ctr, opt)
  | Pexp_variant _ -> raise (NotImplemented "variant")
  | Pexp_record _ -> raise (NotImplemented "Pexp_record")
  | Pexp_ifthenelse _ -> raise (NotImplemented "ifthenelse")
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
  | Pstr_value (rec_flag, bindings) -> raise (NotImplemented "let without in")
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
  | x :: xs -> dom_struct_item x

and dom_int_block n = dom_block "int_typed" [dom_field "INT" (string_of_int n)]

and dom_float_block n = dom_block "float_typed" [dom_field "Float" (string_of_float n)]

and dom_id_block id = dom_block "variables_get_typed" [dom_var_field "VAR" false id]

and dom_bool_block isTrue =
  let upper_value = if isTrue then "TRUE" else "FALSE" in
  dom_block "logic_boolean_typed" [dom_field "BOOL" upper_value]

and dom_let_block rec_flag binding exp2 = match (rec_flag, binding, exp2) with
  | (Recursive, _, _) -> raise (NotImplemented "Letrec")
  | (Nonrecursive, {pvb_pat=patt; pvb_expr=exp1},  _) ->
    match patt.ppat_desc with
      | Ppat_var var ->
        let field = dom_var_field "VAR" true var.txt in
        let domExp1 = dom_expr exp1 in
        let domExp2 = dom_expr exp2 in
        let dom = dom_block "let_typed" [field] in
        let dom = append_value dom "EXP1" domExp1 in
        let dom = append_value dom "EXP2" domExp2 in
        dom
      | _ -> raise (NotImplemented "pattern in let")

and dom_app_lst_block exp1 exp2 exp2_rest =
  let left = dom_app_block exp1 exp2 in
  List.fold_left (fun dom exp -> dom_app_block' dom (dom_expr exp)) left exp2_rest

and dom_app_block' dom_exp1 dom_exp2 =
  let dom = dom_block "lambda_app_typed" [] in
  let dom = append_value dom "FUN" dom_exp1 in
  let dom = append_value dom "ARG" dom_exp2 in
  dom

and dom_app_block expr1 expr2 =
  let domExp1 = dom_expr expr1 in
  let domExp2 = dom_expr expr2 in
  dom_app_block' domExp1 domExp2

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

and dom_block typeName children =
  Xml.createDom "block" [("type", typeName)] children

and dom_block_value name child =
  Xml.createDom "value" [("name", name)] [child]

and append_value xml name child =
  Xml.appendChild xml (dom_block_value name child)

and dom_field name text =
  Xml.createDom "field" [("name", name)] [Xml.createTextDom text]

and dom_var_field name isValue text =
  let field = dom_field name text in
  Xml.setAttribute field ("isvalue", if isValue then "true" else "false")
