open Parsetree
open Asttypes
open Longident

exception NotImplemented of string

let pp = Format.fprintf
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html#TYPEexpression *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Asttypes.html#TYPErec_flag *)

(* miscellaneous printers *)
(* TODO move these *)
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
  | Lident (name) -> raise (NotImplemented "Lident")
    (* i . i *) (** Access in module *)
  | Ldot (ident, name) -> raise (NotImplemented "LDot")
    (* i i *) (** Application *)
  | Lapply (ident1, ident2) -> raise (NotImplemented "Lapply")

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
  | Pexp_let (rec_flag, bindings, expr) -> dom_let_block rec_flag bindings expr
  | Pexp_fun _ -> raise (NotImplemented "fun")
  | Pexp_apply _ -> raise (NotImplemented "apply")
  | Pexp_match _ -> raise (NotImplemented "match")
  | Pexp_try _ -> raise (NotImplemented "try")
  | Pexp_tuple _ -> raise (NotImplemented "tuple")
  | Pexp_variant _ -> raise (NotImplemented "variant")
  | Pexp_record _ -> raise (NotImplemented "Pexp_record")
  | Pexp_ifthenelse _ -> raise (NotImplemented "ifthenelse")
  | _ -> raise (NotImplemented "expr")

(* The type of structure items *)
and dom_struct_item item = match item.pstr_desc with
  | Pstr_eval (expression, _) -> dom_expr expression
  | Pstr_value (rec_flag, bindings) -> raise (NotImplemented "Pstr_value")
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

and dom_list : 'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    = fun ppr f xs -> raise (NotImplemented "dom_list")

and dom_list' : 'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    = fun ppr f xs -> match xs with
        | [] -> raise (NotImplemented "dom_list' empty")
        | x::r -> raise (NotImplemented "dom_list' cons")

and dom_int_block n = dom_block "int_typed" [dom_field "INT" (string_of_int n)]

and dom_id_block id = dom_block "variables_get_typed" [dom_var_field "VAR" false id]

and dom_bool_block isTrue =
    let upper_value = if isTrue then "TRUE" else "FALSE" in
    dom_block "logic_boolean_typed" [dom_field "BOOL" upper_value]

and dom_let_block rec_flag bindings expr = match (rec_flag, bindings, exp) with
  | (Recursive, _, _) -> raise (NotImplemented "Letrec")
  | (Nonrecursive, _, _) -> raise (NotImplemented "Let")

and dom_app_block expr1 expr2 =
    let domExp1 = dom_expr expr1 in
    let domExp2 = dom_expr expr2 in
    let dom = dom_block "lambda_app_typed" [] in
    let dom = append_value dom "FUN" domExp1 in
    let dom = append_value dom "ARG" domExp2 in
    dom

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
