
open Nonstd
module String = Sosa.Native_string

let dbg fmt =
  ksprintf (fun s ->
      Firebug.console##info (Js.string s)
    ) fmt

module Reactive_source = struct
  type 'a t = {
    signal: 'a React.S.t;
    set: 'a -> unit;
  }
  let create ?eq v =
    let signal, set = React.S.create ?eq v in
    {signal; set}
  let signal t = t.signal
  let set t v = t.set v
end


module Graph = struct
  module Node_id = struct
    type t = string
    let eq : t -> t -> bool = fun a b -> a = b
  end

  module Node = struct
    type t = {
      id: Node_id.t [@main];
      forward: Node_id.t list;
      backwards: Node_id.t list;
    } [@@deriving make]
    let eq a b = Node_id.eq a.id b.id
  end
  module Node_table = struct

    type t = (Node_id.t, Node.t option Reactive_source.t) Hashtbl.t 

    let get t id =
      match Hashtbl.find t id with
      | v -> Reactive_source.signal v
      | exception _ ->
        let src =
          let eq a =
            function
            | None -> a = None
            | Some v1 ->
              begin match a with
              | None -> false
              | Some v2 -> Node.eq v1 v2
              end in
          Reactive_source.create ~eq None in
        Hashtbl.add t id src;
        Reactive_source.signal src
  end

  module Download_node = struct
    type t = {url: string}
    let get t id =
      Lwt.(
        Lwt_js.sleep (Random.float 2.0)
        >>= fun () ->
        (* let fake_node = *)
        (*   Node.make (\* TODO *\) *)
        return ()
      )
  end

  type t = {
    nodes: Node_table.t;
    download: Download_node.t;
  }

  let get_node t id =
    Node_table.get t.nodes id


end

module Drawing = struct

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
    c##closePath;
    ()

  let clickable_div ?(debug_border = true) c x y w h ~f =
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
    if debug_border then
      div##.style##.borderStyle := Js.string "dotted";
    div##.onclick := Dom_html.handler (fun _ ->
        f ();
        Js._false);
    (* let parent = *)
    (*   Js.Opt.get *)
    (*     c##.canvas##.parentNode *)
    (*     (fun () -> dbg "canvas has no parent?"; failwith "NONE") *)
    (* in *)
    div##.innerHTML := Js.string "Boh";
    c##appendChild (div :> Dom.node Js.t);
    div

end


module Board = struct

  let create_canvas w h =
    let c = Dom_html.createCanvas Dom_html.document in
    c##.width := w;
    c##.height := h;
    c

  type position = {x : int; y: int}
  let pos x y = {x; y}

  type drag_events = {
    mouse_down: position React.E.t;
    mouse_drag: position React.E.t;
    mouse_up: position React.E.t;
  }

  type t = {
    canvas: Dom_html.canvasElement Js.t;
    parent: Dom_html.element Js.t;
    context: Dom_html.canvasRenderingContext2D Js.t;
    mouse: drag_events;
  }

  let drag_events canvas =
    let mouse_down, e_mdown_fire = React.E.create () in
    let mouse_up, e_mup_fire = React.E.create () in
    let mouse_drag, e_mdrag_fire = React.E.create () in
    canvas##.onmousedown := Dom_html.handler (fun ev ->
        let x0 = ev##.clientX and y0 = ev##.clientY in
        (* t.canvas##.style##.cursor := Js.string "move"; *)
        e_mdown_fire (pos x0 y0);
        let c1 =
          Dom_html.(
            addEventListener document Event.mousemove
              (handler (fun ev ->
                   let x = ev##.clientX and y = ev##.clientY in
                   e_mdrag_fire (pos x y);
                   canvas##.style##.cursor := Js.string "move";
                   Dom_html.stopPropagation ev;
                   Js._true))
              Js._true) in
        let c2 = ref Js.null in
        c2 := Js.some Dom_html.(
            addEventListener document Event.mouseup
              (handler (fun ev ->
                   removeEventListener c1;
                   Js.Opt.iter !c2 removeEventListener;
                   canvas##.style##.cursor := Js.string "";
                   let x = ev##.clientX and y = ev##.clientY in
                   e_mup_fire (pos x y);
                   Js._true))
              Js._true);
        Js._true);
    {mouse_down; mouse_drag; mouse_up;}

  let create ~width ~height =
    (* let w = from##.clientWidth in *)
    (* let h = from##.clientHeight in *)
    let canvas = create_canvas width height in
    let main_div = Dom_html.createDiv Dom_html.document in
    main_div##.style##.width := ksprintf Js.string "%dpx" width;
    main_div##.style##.height := ksprintf Js.string "%dpx" height;
    main_div##.style##.borderStyle := Js.string "dotted";
    main_div##.style##.position := Js.string "relative";
    canvas##.style##.backgroundColor := Js.string "#ded";
    Dom.appendChild main_div canvas;
    let context = canvas##getContext Dom_html._2d_ in
    let m = {
      canvas; parent = main_div; context;
      mouse = drag_events canvas;
    } in
    m

  let attach_element {parent; _} e =
    Dom.appendChild parent e

  let context {context; _} = context

  let as_div t = t.parent

  let clear {context; canvas; parent; _} =
    let w, h =
      (float canvas##.clientWidth), (float canvas##.clientHeight) in
    context##clearRect 0. 0. w h;
    let rec rms () =
      Js.Opt.iter parent##.lastChild (fun c ->
          Dom.removeChild parent c;
          rms ()
        )
    in
    rms ();
    Dom.appendChild parent canvas;
    ()

  let translate_image {context; canvas; _} x y =
    (* context##save; *)
    let w, h =
      (float canvas##.clientWidth), (float canvas##.clientHeight) in
    dbg "translate_image canvas: %f %f, → %d, %d" w h x y;
    (*
       if x < 0 then take from (- x) else from 0, put at x;
    *)
    let take_from_x = if x < 0 then - x else 0 in
    let put_at_x = if x < 0 then 0 else x in
    let take_from_y = if y < 0 then - y else 0 in
    let put_at_y = if y < 0 then 0 else y in
    let imgdata =
      context##getImageData
        (float take_from_x) (float take_from_y)
        (w -. float take_from_x) (h -. float take_from_y) in
    (* dbg "imgdata: %s" (Js.Opt.get imgdata##.nodeValue |> Js.to_string); *)
    context##clearRect 0. 0. w h;
    (* context##translate (float x) (float y); *)
    context##putImageData imgdata (float put_at_x) (float put_at_y);
    (* context##drawImage canvas 0. 0.; *)
    (* context##restore *)
    ()

end

module Entity = struct

  type t = {
    render: x: int -> y: int -> Board.t -> unit;
  }
  let create render = {render}

  (* some example entities *)
  let rectangle ~w ~h ?(radius = 3) ?on_click () =
    create begin fun ~x ~y board ->
      Drawing.roundRectPath
        (Board.context board)
        (float x) (float y) (float w) (float h) (float radius) `Stroke;
      begin match on_click  with
      | None -> ()
      | Some f ->
        let _ =
          Drawing.clickable_div
            ~debug_border:true
            (Board.as_div board)
            x y w h
            ~f
        in
        ()
      end

    end

end

module Scene = struct
  type t = {
    draw_on: Board.t;
    mutable objects: (int * int * Entity.t) list;
  }

  let render_base draw_on objects =
    Board.clear draw_on;
    List.iter objects ~f:(fun (x, y, {Entity.render}) -> render ~x ~y draw_on)

  let translate_objects t dx dy =
    t.objects <- List.map t.objects ~f:(fun (x, y, e) -> (x + dx, y + dy, e))

  let create ~draw_on objects =
    let t = {draw_on; objects} in
    let current_translation, set_current_translation =
      React.S.create `None in
    Board.(
      let _ =
        React.E.map (fun {x; y} ->
            dbg "Mouse down %d, %d" x y;
            set_current_translation (`Init (x, y));
          )
          draw_on.mouse.mouse_down
      in
      let _ =
        React.E.map (fun {x; y} ->
            dbg "Mouse up %d, %d" x y;
            render_base draw_on t.objects;
            set_current_translation `Done
          )
          draw_on.mouse.mouse_up
      in
      let _ =
        React.E.map (fun {x; y} ->
            dbg "Mouse drag %d, %d" x y;
            match React.S.value current_translation with
            | `Init (x0, y0) ->
              set_current_translation (`In_progress (x - x0, x, y - y0, y, 0));
            | `In_progress (dx, x0, dy, y0, count) when count <= 7 ->
              set_current_translation (`In_progress (dx + x - x0, x, dy + y - y0, y, count + 1));
            | `In_progress (dx, x0, dy, y0, count) ->
              let dx, dy = (dx + x - x0), (dy + y - y0) in
              dbg "In_progress, translating: %d %d" dx dy;
              Board.translate_image draw_on dx dy;
              translate_objects t dx dy;
              set_current_translation (`Init (x, y))
            | `None | `Done ->
              dbg "THIS SHOULD NOT HAPPEN?"
          )
          draw_on.mouse.mouse_drag
      in
      ()
    );
    t

  let render t = render_base t.draw_on t.objects
end




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

let clickable_div ?(debug_border = true) c x y w h =
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
  if debug_border then
    div##.style##.borderStyle := Js.string "dotted";
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
  roundRectPath c 300. 200. 100. 200. 15.01 `Stroke;
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
  (* let base_div = *)
  (*   Dom_html.getElementById "dibigrob-hook" in *)
  let w = page##.clientWidth in
  let h = page##.clientHeight in
  let canvas = Board.create_canvas w h in
  (* let rendering_context = canvas##getContext Dom_html._2d_ in *)
  (* canvas##.style := Js.string "border : 1"; *)
  canvas##.style##.backgroundColor := Js.string "#eee";
  draw_square (canvas##getContext(Dom_html._2d_));
  (canvas :> Dom.node Js.t)


let attach_to_page () =
  let base_div =
    Dom_html.getElementById "dibigrob-hook" in
  (* some artificial padding *)
  let another_div = Dom_html.createDiv Dom_html.document in
  another_div##.style##.width := Js.string "100px";
  another_div##.style##.height := Js.string "50px";
  another_div##.style##.borderStyle := Js.string "dotted";
  Dom.appendChild base_div another_div;
  (* test the actual API: *)
  let board = Board.create ~width:700 ~height:600 in
  Dom.appendChild base_div (Board.as_div board);
  let scene =
    Scene.create ~draw_on:board [
      10, 5, Entity.rectangle ~w:100 ~h:50 ();
      100, 50, Entity.rectangle ~w:100 ~h:50 ();
      300, 50,
      Entity.rectangle
        ~on_click:(fun () -> dbg "Rectangle clicked!")
        ~w:100 ~h:50 ();
    ] in
  Scene.render scene;
  (* Keep the tests around *)
  let yet_another_div = Dom_html.createDiv Dom_html.document in
  Dom.appendChild yet_another_div (page ());
  Dom.appendChild base_div yet_another_div;
  yet_another_div##.style##.position := Js.string "relative";
  let _ =
    clickable_div yet_another_div 300 200 100 200 in
  Lwt.return ()

let go _ =
  ignore Lwt.(
      catch begin fun () ->
        dbg "Started!";
        attach_to_page ()
        >>= fun () ->
        return ()
      end (fun exn ->
          Printf.ksprintf
            (fun s -> Firebug.console##error (Js.string s); failwith s)
            "Uncaught Exception: %s" (Printexc.to_string exn)));
  Js._true

let _ =
  Dom_html.window##.onload := Dom_html.handler go
