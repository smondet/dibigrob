
open Nonstd
module String = Sosa.Native_string

let dbg fmt =
  ksprintf (fun s ->
      Firebug.console##info (Js.string s)
    ) fmt


let create_canvas w h =
  let c = Dom_html.createCanvas Dom_html.document in
  c##.width := w;
  c##.height := h;
  c


let page () =
  let canvas = create_canvas 300 200 in
  (* let rendering_context = canvas##getContext Dom_html._2d_ in *)
  (* canvas##.style := Js.string "border : 1"; *)
  canvas##.style##.backgroundColor := Js.string "blue";
  (canvas :> Dom.node Js.t)


let attach_to_page gui =
  let base_div =
    Dom_html.getElementById "dibigrob-hook" in
  base_div##appendChild gui |> ignore;
  Lwt.return ()

let go _ =
  ignore Lwt.(
      catch begin fun () ->
        dbg "Started!";
        attach_to_page (page ())
        >>= fun () ->
        return ()
      end (fun exn ->
          Printf.ksprintf
            (fun s -> Firebug.console##error (Js.string s); failwith s)
            "Uncaught Exception: %s" (Printexc.to_string exn)));
  Js._true

let _ =
  Dom_html.window##.onload := Dom_html.handler go
