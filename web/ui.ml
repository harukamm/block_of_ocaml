let window = Dom_html.window

let document = Dom_html.window##document

let white = "#FFFFFF"

let to_JsBool value =
  if value then Js._true else Js._false

let change_background_color element color =
  element##style##backgroundColor <- Js.string color

let create_button txt handler =
  let b = Dom_html.createInput ~_type:(Js.string "submit") document in
  change_background_color b white;
  b##value <- Js.string txt;
  b##onclick <- Dom_html.handler handler;
  b

let create_textarea read_only =
  let textarea = Dom_html.createTextarea document in
  textarea##readOnly <- (to_JsBool read_only);
  textarea##value <- Js.string "";
  textarea##cols <- 80;
  textarea##rows <- 10;
  textarea

let create_br() =
  Dom_html.createBr document

let get_converter_div() =
  Js.Opt.get (document##getElementById(Js.string "converter"))
      (fun () -> assert false)

let converter_for_js input =
  let input' = Js.to_string input in
  match (Main.block_of_ocaml input') with
    | (Some str, None) ->
      Js.string str
    | (None, None) ->
      Js.string "nande~~"
    | (_, Some (AstToBlock.NotImplemented(ctr))) ->
      Js.string ("notImplementedAST" ^ ctr)
    | (_, Some e) ->
      Js.string "unknown_error"

let get_xml_out () =
  match !My_compile.xml_out with
    | None -> Js.string "nande~~><"
    | Some str -> Js.string str

let on_click_OK input_textarea output_textarea _ =
  let input = input_textarea##value in
  let output = converter_for_js input in
  output_textarea##value <- output;
  to_JsBool true

(*
let onload _ =
  let div = get_converter_div() in
  let input_textarea = create_textarea false in
  let output_textarea = create_textarea true in
  let wrapper = on_click_OK input_textarea output_textarea in
  let ok_button = create_button "OK" wrapper in
  Dom.appendChild div input_textarea;
  Dom.appendChild div (create_br());
  Dom.appendChild div ok_button;
  Dom.appendChild div (create_br());
  Dom.appendChild div output_textarea;
  to_JsBool true

let () = ignore begin
  Dom_html.addEventListener
    Dom_html.window
    Dom_html.Event.load
    (Dom_html.handler onload)
    Js._true
end
*)

let run_main input =
  let filename = Js.to_string input in
  Main.main filename

let () =
  Js.Unsafe.global##blockOfOCaml <- converter_for_js;;
  Js.Unsafe.global##getXmlOut <- get_xml_out;;
  Js.Unsafe.global##runMain <- run_main
;;
