open Claudius
open Tsdl
open Tsdl_ttf

type star = {
  x : int;
  y : int;
  brightness : int;
}

let generate_stars num_stars width height =
  let rand_brightness () = 1 + Random.int 15 in
  List.init num_stars (fun _ -> { x = Random.int width; y = Random.int height; brightness = rand_brightness () })

let twinkle stars =
  List.map (fun star -> 
    { star with 
      brightness = max 1 ((star.brightness + (Random.int 3 - 1)) mod 16)
    }
  ) stars

(* FPS Calculation *)
let last_time = ref (Sdl.get_ticks ())
let frame_count = ref 0
let fps_counter = ref 0

let calculate_fps () =
  let now = Sdl.get_ticks () in
  let elapsed = Int32.sub now !last_time in
  frame_count := !frame_count + 1;
  if elapsed >= 1000l then (
    fps_counter := !frame_count;
    frame_count := 0;
    last_time := now
  )

let render (_stars : star list) : Primitives.t list =
  let star_primitives = 
    List.map (fun star -> Primitives.Pixel ({ x = star.x; y = star.y }, star.brightness)) _stars
  in
  (* Add FPS text as simple pixel art *)
  let fps_text = Printf.sprintf "FPS: %d" !fps_counter in
  let text_primitives = 
    let base_x = 2 in
    let base_y = 2 in
    let draw_char x y c acc = 
      (* Very simple 5x7 font rendering for demo purposes *)
      (* Now drawing each pixel at 2x scale *)
      let draw_pixel px py b acc =
        (* Draw 4 pixels for each original pixel to create 2x scale *)
        let pixels = [
          (px, py, b); (px+1, py, b);
          (px, py+1, b); (px+1, py+1, b)
        ] in
        List.fold_left (fun acc (px, py, b) -> 
          Primitives.Pixel ({ x = px; y = py }, b) :: acc
        ) acc pixels
      in
      match c with
      | 'F' -> 
        let pixels = [
          (x, y); (x+1, y); (x+2, y); (x+3, y);
          (x, y+1);
          (x, y+2); (x+1, y+2); (x+2, y+2);
          (x, y+3);
          (x, y+4)
        ] in
        List.fold_left (fun acc (px, py) -> 
          draw_pixel (px*2) (py*2) 15 acc
        ) acc pixels
      | 'P' ->
        let pixels = [
          (x, y); (x+1, y); (x+2, y); (x+3, y);
          (x, y+1); (x+3, y+1);
          (x, y+2); (x+1, y+2); (x+2, y+2); (x+3, y+2);
          (x, y+3);
          (x, y+4)
        ] in
        List.fold_left (fun acc (px, py) -> 
          draw_pixel (px*2) (py*2) 15 acc
        ) acc pixels
      | 'S' ->
        let pixels = [
          (x, y); (x+1, y); (x+2, y); (x+3, y);
          (x, y+1);
          (x, y+2); (x+1, y+2); (x+2, y+2); (x+3, y+2);
          (x+3, y+3);
          (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
        ] in
        List.fold_left (fun acc (px, py) -> 
          draw_pixel (px*2) (py*2) 15 acc
        ) acc pixels
      | ':' ->
        let pixels = [
          (x+1, y+1);
          (x+1, y+3)
        ] in
        List.fold_left (fun acc (px, py) -> 
          draw_pixel (px*2) (py*2) 15 acc
        ) acc pixels
      | '0'..'9' as digit ->
        let n = Char.code digit - Char.code '0' in
        (* Simple digit rendering *)
        let segments = match n with
          | 0 -> [(0,0);(1,0);(2,0); (0,1);(2,1); (0,2);(2,2); (0,3);(2,3); (0,4);(1,4);(2,4)]
          | 1 -> [(2,0); (2,1); (2,2); (2,3); (2,4)]
          | 2 -> [(0,0);(1,0);(2,0); (2,1); (0,2);(1,2);(2,2); (0,3); (0,4);(1,4);(2,4)]
          | 3 -> [(0,0);(1,0);(2,0); (2,1); (0,2);(1,2);(2,2); (2,3); (0,4);(1,4);(2,4)]
          | 4 -> [(0,0);(2,0); (0,1);(2,1); (0,2);(1,2);(2,2); (2,3); (2,4)]
          | 5 -> [(0,0);(1,0);(2,0); (0,1); (0,2);(1,2);(2,2); (2,3); (0,4);(1,4);(2,4)]
          | 6 -> [(0,0);(1,0);(2,0); (0,1); (0,2);(1,2);(2,2); (0,3);(2,3); (0,4);(1,4);(2,4)]
          | 7 -> [(0,0);(1,0);(2,0); (2,1); (2,2); (2,3); (2,4)]
          | 8 -> [(0,0);(1,0);(2,0); (0,1);(2,1); (0,2);(1,2);(2,2); (0,3);(2,3); (0,4);(1,4);(2,4)]
          | 9 -> [(0,0);(1,0);(2,0); (0,1);(2,1); (0,2);(1,2);(2,2); (2,3); (0,4);(1,4);(2,4)]
          | _ -> []
        in
        List.fold_left (fun acc (dx, dy) -> 
          draw_pixel ((x+dx)*2) ((y+dy)*2) 15 acc
        ) acc segments
      | _ -> acc (* Skip unknown characters *)
    in
    let rec draw_string x y chars acc =
      match chars with
      | [] -> acc
      | c :: cs -> 
        let new_acc = draw_char (x/2) (y/2) c acc in
        draw_string (x + 10) y cs new_acc  (* Increased spacing between characters *)
    in
    draw_string (base_x*2) (base_y*2) (List.of_seq (String.to_seq fps_text)) []
  in
    star_primitives @ text_primitives

let tick (_t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.map (fun _ -> 0) prev in
  let width, height = Screen.dimensions s in
  let stars = generate_stars 100 width height |> twinkle in
  calculate_fps ();
  render stars |> Framebuffer.render buffer;
  buffer

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "SDL init failed: %s" e; exit 1
  | Ok () -> 
    match Ttf.init () with
    | Error (`Msg e) -> Sdl.log "TTF init failed: %s" e; Sdl.quit (); exit 1
    | Ok () -> 
      match Sdl.create_window ~w:400 ~h:400 "Twinkling Stars" Sdl.Window.opengl with
      | Error (`Msg e) -> 
        Sdl.log "Window creation failed: %s" e;
        Ttf.quit (); Sdl.quit (); exit 1
      | Ok window ->
        match Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated with
        | Error (`Msg e) -> 
          Sdl.log "Renderer creation failed: %s" e;
          Sdl.destroy_window window;
          Ttf.quit (); Sdl.quit (); exit 1
        | Ok renderer ->
          let screen = Screen.create 400 400 1 (Palette.generate_mono_palette 16) in
          let tick_wrapper t s p i = 
            let buffer = tick t s p i in
            ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
            ignore (Sdl.render_clear renderer);
            ignore (Sdl.render_present renderer);
            buffer
          in
          Base.run "Twinkling Stars" None tick_wrapper screen;
          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          Ttf.quit ();
          Sdl.quit ()
