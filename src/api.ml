open Archetype
open Js_of_ocaml


let _ =
  let doit f input =
    try
      input
      |> Js.to_string
      |> f
      |> Js.string
    with
    | exn ->
      match Js.Opt.to_option (Js.js_error_of_exn exn) with
      | None -> raise exn
      | Some err -> Js.raise_js_error err
  in
  Options.opt_no_js_header := true;
  Js.export_all
    (object%js
      method compile i = begin
        Archetype.Options.target := Michelson;
        doit Compile.compile_from_string i
      end
      method compileJS i = begin
        Archetype.Options.target := Javascript;
        doit Compile.compile_from_string i
      end
      method getWhyml i = begin
        Archetype.Options.target := Whyml;
        doit Compile.compile_from_string i
      end
      method getParameters i = begin
        doit Compile.get_parameters i
      end
      method showEntries i = begin
        doit Compile.show_entries i
      end
      val version = Js.string Options.version
    end)
