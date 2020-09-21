
module H = Dom_html

let doc = H.document

(** Create a new input line with a prompt *)
let termAddInput t f =
  let line = H.createDiv doc in
  let prompt = H.createDiv doc in
  prompt##.innerHTML := Js.string "&#128049;eq> &nbsp";
  prompt##.className := Js.string "term-prompt";
  let input = H.createInput ~_type:(Js.string "text") doc in
  input##.className := Js.string "term-input";
  input##.onkeypress := H.handler (fun x ->
      (** If one presses ENTER, the input is sent *)
      if x##.keyCode = 13 then
        (
          f (Js.to_string input##.value);
          Js._false
        )
      else Js._true );
  line##.className := Js.string "term-line";
  Dom.appendChild line prompt;
  Dom.appendChild line input;
  Dom.appendChild t line;
  input

(** Create a container for the terminal *)
let termBuild () =
  let container = H.createDiv doc in
  container##.id := Js.string "term-cont";
  container##.className := Js.string "term-cont";
  container

(** Install the terminal in the web page *)
let init_web () =
  let term = termBuild () in
  let div = H.getElementById "term-app" in
  let currentInput = ref None in
  Dom.appendChild div term;
  (** Handler function that send the input to cateq *)
  let rec hdl s =
    let Some curr = !currentInput in
    curr##setAttribute (Js.string "readonly") (Js.string "");
    let cont = H.createDiv doc in
    cont##.innerHTML := Js.string (Cateqlib.handle_input s);
    Dom.appendChild term cont;
    let newinput = termAddInput term hdl in
    newinput##focus;
    currentInput := Some newinput
  in
  let newinput = termAddInput term hdl in
  newinput##focus;
  currentInput := Some newinput;
  term##.onclick := H.handler (fun _ -> let Some curr = !currentInput in
                             curr##focus;
                             Js._true
                              )

(** Callback the init function when the page is loaded *)
let _ =
  H.window##.onload := H.handler (fun _ -> init_web () ; Js._true)
