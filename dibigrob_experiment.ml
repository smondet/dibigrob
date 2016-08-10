
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

let roundRectPath (c: Dom_html.canvasRenderingContext2D Js.t) x y w h r kind =
  let r = min r (min w h /. 2.) in
  c##beginPath;
  c##moveTo (x +. r)  y;
  c##arcTo (x +. w) y (x +. w) (y +. r) r;
  c##arcTo (x +. w) (y +. h) ( x +. w -. r) (y +. h) r;
  c##arcTo x (y +. h) x (y +. h -. r) r;
  c##arcTo x y (x +. r) y r;
  begin match kind with
  | `Fill ->
    c##fill;
  | `Stroke -> c##stroke
  end;
  (* c##addHitRegion ; *)
  c##closePath;
  ()

let clickable_div c x y w h =
  let div = Dom_html.createDiv Dom_html.document in
  div##.id := ksprintf Js.string "test-clickable_div-%d-%d-%d-%d" x y w h;
  div##.style##.position := Js.string "absolute";
  div##.style##.width := ksprintf Js.string "%dpx" w;
  div##.style##.height :=
    ksprintf Js.string "%dpx" h;
  div##.style##.left := ksprintf Js.string "%dpx" x;
  div##.style##.top :=
    ksprintf Js.string "%dpx" y;
  div##.style##.cursor := Js.string "pointer";
  div##.style##.zIndex := Js.string "2";
  div##.onclick := Dom_html.handler (fun _ ->
      dbg "clickable_div clicked !!";
      Js._false);
  (* let parent = *)
  (*   Js.Opt.get *)
  (*     c##.canvas##.parentNode *)
  (*     (fun () -> dbg "canvas has no parent?"; failwith "NONE") *)
  (* in *)
  div##.innerHTML := Js.string "Boh";
  c##appendChild (div :> Dom.node Js.t);
  div


  
let draw_square c =
  c##.fillStyle := Js.string "rgba(200, 0, 200, 0.5)";
  c##.strokeStyle := Js.string "rgba(200, 0, 0, 0.5)";
  roundRectPath c 100.5 100.6 100.2 100.3 10.01 `Fill;
  c##.lineWidth := 4.;
  roundRectPath c 300. 600. 100. 200. 5.01 `Stroke;
  c##.fillStyle := Js.string "rgb(200,0,0)";
  c##fillRect 10. 10. 50. 50.;
  c##.fillStyle := Js.string "rgba(0, 0, 200, 0.5)";
  c##fillRect 30. 30. 50. 50.;
  c##.font := Js.string "48px serif";
  let text = (Js.string "Hello world") in
  c##fillText text 20. 400.;
  let mesu = c##measureText text in
  roundRectPath c 10. 340.6 (mesu##.width +. 20.) (48. +. 50.) 30.01 `Stroke;
  ()

let page () =
  let page = Dom_html.document##.documentElement in
  let w = page##.clientWidth in
  let h = page##.clientHeight in
  let canvas = create_canvas w h in
  (* let rendering_context = canvas##getContext Dom_html._2d_ in *)
  (* canvas##.style := Js.string "border : 1"; *)
  canvas##.style##.backgroundColor := Js.string "#eee";
  draw_square (canvas##getContext(Dom_html._2d_));
  (canvas :> Dom.node Js.t)


let attach_to_page gui =
  let base_div =
    Dom_html.getElementById "dibigrob-hook" in
  base_div##appendChild gui |> ignore;
  let _ =
    clickable_div base_div 300 600 100 200 in
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
