open Parsetree
open Asttypes
open Longident

exception NotImplemented of string

(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html#TYPEexpression *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Asttypes.html#TYPErec_flag *)
(* Reference: https://github.com/davidlazar/xml_of_ocaml *)

type arg_t =
  | Argument of Asttypes.arg_label *
      Parsetree.expression option * Parsetree.pattern

type variable_label =
  | Variable
  | Constructor
  | Record
  | Record_field

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
and dom_pattern patt = match patt.ppat_desc with
  | Ppat_any -> raise (NotImplemented "Any")
  | Ppat_var var -> dom_var_field "VAR" true var.txt
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
  | Pexp_match (exp, cases) -> dom_match exp cases
  | Pexp_try _ -> raise (NotImplemented "try")
  | Pexp_tuple (x :: y :: rest) ->
    if List.length rest = 0 then
      dom_tuple_block x y
    else
      raise (NotImplemented "n-tuple (n < 2)")
  | Pexp_tuple _ -> assert false
  | Pexp_construct ({txt=Lident "::"}, Some {pexp_desc=Pexp_tuple lst}) ->
    let lst' = flatten_list_ctor expr in
    dom_list_block lst'
  | Pexp_construct (ctor, opt) -> dom_construct (ctor, opt)
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
  | Pstr_type (rec_flag, type_declarations) ->
    if List.length type_declarations = 1 then
      let first = List.nth type_declarations 0 in
      dom_type_declaration first
    else
      raise (NotImplemented "Mutually recursive types")
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

and dom_type_declaration type_declaration =
  let dataname = type_declaration.ptype_name.txt in
  if List.length type_declaration.ptype_params <> 0 then
    raise (NotImplemented "Type declaration with parameters");
  if List.length type_declaration.ptype_cstrs <> 0 then
    raise (NotImplemented "constraint");
  if type_declaration.ptype_private = Private then
    raise (NotImplemented "Private type declaration");
  if List.length type_declaration.ptype_attributes <> 0 then
    raise (NotImplemented "Type declaration with attributes");
  let manifest = type_declaration.ptype_manifest in
  match type_declaration.ptype_kind with
  | Ptype_abstract -> raise (NotImplemented "Abstract type declaration")
  | Ptype_variant lst -> dom_constructor_declaration dataname manifest lst
  | Ptype_record lst -> dom_record_declaration dataname manifest lst
  | Ptype_open -> raise (NotImplemented "open")

and dom_constructor_declaration name manifest lst =
  if manifest <> None then
    raise (NotImplemented "Constructor declaration with manifest");
  assert (List.length lst <> 0);
  let mutation = items_mutation (List.length lst) in
  let dataname_xml = dom_field "DATANAME" name in
  let rec create_ctors_xml lst_ i =
    match lst_ with
    | [] -> ([], [])
    | ctor :: rest ->
      if List.length ctor.pcd_attributes != 0 then
        raise (NotImplemented "Constructor declaration with attributes");
      if ctor.pcd_res <> None then
        raise (NotImplemented "pcd_res");
      let opt_arg_xml = dom_constructor_argument ctor.pcd_args ctor.pcd_loc in
      let i_str = string_of_int i in
      let ctor_name = ctor.pcd_name.txt in
      let field = dom_var_field ("CTR" ^ i_str) true
        ~var_type:Constructor ctor_name in
      let (fields, values) = create_ctors_xml rest (i + 1) in
      let values' =
        match opt_arg_xml with
        | None -> values
        | Some (arg_xml) ->
          let value = dom_block_value ("CTR_INP" ^ i_str) arg_xml in
          value :: values
      in
      (field :: fields, values')
  in
  let (fields, values) = create_ctors_xml lst 0 in
  let ctors_xml = fields @ values in
  let children = mutation :: dataname_xml :: ctors_xml in
  dom_block "defined_datatype_typed" children

and dom_constructor_argument arg dummy_loc =
  match arg with
  | Pcstr_tuple types ->
    let size = List.length types in
    if size = 0 then
      None
    else if size = 1 then
      Some (dom_core_type (List.hd types))
    else if size = 2 || size = 3 then
      (* Create core_type indicating tuples. *)
      let dummy_tuple = {ptyp_desc=Ptyp_tuple (types);
        ptyp_attributes=[]; ptyp_loc=dummy_loc} in
      Some (dom_core_type dummy_tuple)
    else raise (NotImplemented ("Constructor declaration with more " ^
      "than 4 tuples"))
  | Pcstr_record label_declarations ->
    raise (NotImplemented "Constructor declaration with records")

and dom_record_declaration name manifest lst =
  if manifest <> None then
    raise (NotImplemented "Record declaration with manifest");
  assert (List.length lst <> 0);
  let mutation = items_mutation (List.length lst) in
  let dataname_field = dom_var_field "DATANAME" true
    ~var_type:Record name in
  let rec create_labels_xml lst_ i =
    match lst_ with
    | [] -> ([], [])
    | l :: rest ->
      if List.length l.pld_attributes != 0 then
        raise (NotImplemented "Record field declaration with attributes");
      if l.pld_mutable = Mutable then
        raise (NotImplemented "Mutable record field");
      let type_xml = dom_core_type l.pld_type in
      let i_str = string_of_int i in
      let label_name = l.pld_name.txt in
      let field = dom_var_field ("FIELD" ^ i_str) true
        ~var_type:Record_field label_name in
      let value = dom_block_value ("FIELD_INP" ^ i_str) type_xml in
      let (fields, values) = create_labels_xml rest (i + 1) in
      (field :: fields, value :: values)
  in
  let (fields, values) = create_labels_xml lst 0 in
  let children = mutation :: dataname_field :: (fields @ values) in
  dom_block "defined_recordtype_typed" children

and dom_core_type core_type =
  if List.length core_type.ptyp_attributes <> 0 then
    raise (NotImplemented "Types with attributes");
  match core_type.ptyp_desc with
  | Ptyp_any -> raise (NotImplemented "Any types")
  | Ptyp_var str -> raise (NotImplemented "Type variables")
  | Ptyp_arrow (label, typ1, typ2) -> raise (NotImplemented "Arrow types")
  | Ptyp_tuple lst ->
    let size = List.length lst in
    assert (1 < size);
    if 3 < size then
      raise (NotImplemented ("Tuple types with more than 3 arugments"))
    else if size = 2 then
      let xmls = List.map dom_core_type lst in
      let left = dom_block_value "LEFT" (List.nth xmls 0) in
      let right = dom_block_value "RIGHT" (List.nth xmls 1) in
      let values = [left; right] in
      dom_block "pair_type_constructor_typed" values
    else
      let xmls = List.map dom_core_type lst in
      let item0 = dom_block_value "ITEM0" (List.nth xmls 0) in
      let item1 = dom_block_value "ITEM1" (List.nth xmls 1) in
      let item2 = dom_block_value "ITEM2" (List.nth xmls 2) in
      let values = [item0; item1; item2] in
      dom_block "triple_type_constructor_typed" values
  | Ptyp_constr (loc, types) ->
    begin
      let ident = loc.txt in
      match ident with
      | Lident id ->
        let supported = ["int"; "float"; "bool"; "string"] in
        if not (List.mem id supported) then
          raise (NotImplemented ("Type construction " ^ id));
        let name = id ^ "_type_typed" in
        dom_block name []
      | Ldot _ -> raise (NotImplemented ("Type construction Ldot"))
      | Lapply _ -> raise (NotImplemented ("Type construction Lapply"))
    end
  | Ptyp_object (obj_fields, flag) -> raise (NotImplemented "Object types")
  | Ptyp_class (loc, typs) -> raise (NotImplemented "Class types")
  | Ptyp_alias (typ, str) -> raise (NotImplemented "Alias types")
  | Ptyp_variant (fields, flag, opt_label) -> raise (NotImplemented "Variant types")
  | Ptyp_poly (locs, typ) -> raise (NotImplemented "Poly types")
  | Ptyp_package pkg -> raise (NotImplemented "Package types")
  | Ptyp_extension xtn -> raise (NotImplemented "Extension types")

and dom_case case = match case with
  | {pc_lhs=left; pc_guard=Some _; pc_rhs=right} ->
    raise (NotImplemented "Pattern with guard")
  | {pc_lhs=left; pc_guard=None; pc_rhs=right} ->
    (dom_pattern left, dom_expr right)

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

and dom_match expr cases =
  let case_size = List.length cases in
  let mutation = items_mutation case_size in
  let rec h i case_list = match case_list with
    | [] -> []
    | case :: rest ->
      let (l, r) = dom_case case in
      let i_str = string_of_int i in
      let valuel = dom_block_value ("PATTERN" ^ i_str) l in
      let valuer = dom_block_value ("OUTPUT" ^ i_str) r in
      valuel :: valuer :: (h (i + 1) rest)
  in
  let values = h 0 cases in
  dom_block "match_typed" (mutation :: values)

and params_mutation n =
  Xml.createDom "mutation" [("params", string_of_int n)] []

and items_mutation n =
  Xml.createDom "mutation" [("items", string_of_int n)] []

and dom_empty_list () =
  let mutation = items_mutation 0 in
  dom_block "lists_create_with_typed" [mutation]

and flatten_list_ctor exp =
  match exp.pexp_desc with
    | Pexp_construct ({txt=Lident ("[]")}, None) -> []
    | Pexp_construct ({txt=Lident ("::")}, Some {pexp_desc=Pexp_tuple (e1 :: [e2])}) ->
      e1 :: (flatten_list_ctor e2)
    | _ -> assert false

and arguments_mutation names =
  let children = List.map (fun name ->
      Xml.createDom "item" [] [Xml.createTextDom name]) names in
  Xml.createDom "mutation" [] children

and dom_list_block exprs =
  let rec h es i = match es with
    | [] -> []
    | expr :: rest ->
      let i_str = string_of_int i in
      let value = dom_block_value ("ADD" ^ i_str) (dom_expr expr) in
      value :: (h rest (i + 1))
  in
  let mutation = items_mutation (List.length exprs) in
  let values = h exprs 0 in
  dom_block "lists_create_with_typed" (mutation :: values)

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

and dom_var_field name is_value ?(var_type=Variable) text =
  let field = dom_field name text in
  let is_value = if is_value then "true" else "false" in
  let field = Xml.setAttribute field ("isvalue", is_value) in
  Xml.setAttribute field ("variable-type", label_to_name var_type)

(* Note: Keep consistency with label names defined in OCaml Blockly. *)
(* See https://github.com/harukamm/ocaml-blockly/blob/master/core/bound_variable_abstract.js. *)
and label_to_name : variable_label -> string = function
  | Variable -> "variable"
  | Constructor -> "constructor"
  | Record -> "record"
  | Record_field -> "record-field"
