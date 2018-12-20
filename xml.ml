type xml =
  | Element of (string * (string * string) list * xml list)
  | Text of string
  | Nil

let createDom tagName attr children =
  Element (tagName, attr, children)

let createTextDom text =
  Text text

let createNilDom () =
  Nil

let appendChild container child = match container with
  | Element (tagName, attrs, childNodes) ->
    Element (tagName, attrs, childNodes @ [child])
  | Text _
  | Nil -> failwith "Can't append xml node to text/nil node."

let appendText container text =
  appendChild container (createTextDom text)

let setAttribute xml attr = match xml with
  | Element (tagName, attrs, childNodes) ->
    Element (tagName, attrs @ [attr], childNodes)
  | Text _
  | Nil -> failwith "Can't set attribute to text/nil node."

let escape_map =
  [('&', "&amp;"); ('<', "&lt;"); ('>', "&gt;"); ('\'', "&apos;");
   ('\"', "&quot;")]
let escape_targets = List.map fst escape_map

let escape_string str =
  let len = String.length str in
  let sub f l = String.sub str f (l - f + 1) in
  let rec step i prev =
    if i < 0 || len < i then
      assert false
    else if i = len then
      sub prev (i - 1)
    else
      let c = String.get str i in
      if List.mem c escape_targets then
        let substr = if i = 0 then "" else sub prev (i - 1) in
        let s = List.assoc c escape_map in
        substr ^ s ^ (step (i + 1) (i + 1))
      else
        step (i + 1) prev
  in
  step 0 0

let printAttr (prop, value) =
  prop ^ "=\"" ^ (escape_string value) ^ "\""

let rec printAttrs attrs = match attrs with
  | [] -> ""
  | x :: xs ->
    (printAttr x) ^ (if xs = [] then "" else " " ^ (printAttrs xs))

let rec repeat_space n = "" (* if n <= 0 then "" else " " ^ (repeat_space (n - 1)) *)

let rec print' depth xml =
  let ident = repeat_space depth in
  match xml with
  | Element (tagName, attrs, childNodes) ->
    let attrStr = printAttrs attrs in
    let childStr =
      childNodes
      |> List.map (print' (depth + 1))
      |> List.fold_left (fun str x -> str ^ x) ""
    in
    if attrs = [] then
      ident ^ "<" ^ tagName ^ ">" ^ childStr ^
      ident ^ "</" ^ tagName ^ ">"
    else
      ident ^ "<" ^ tagName ^ " " ^ attrStr ^ ">" ^ childStr ^
      ident ^ "</" ^ tagName ^ ">"
  | Text (text) ->
    ident ^ (escape_string text)
  | Nil ->
    ""

let print = print' 0
