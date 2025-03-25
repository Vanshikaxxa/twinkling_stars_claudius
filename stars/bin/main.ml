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

let render (_s : Screen.t) (stars : star list) : Primitives.t list =
  List.map (fun star -> Primitives.Pixel ({ x = star.x; y = star.y }, star.brightness)) stars

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

let load_font () =
  let font_paths = [
    "/usr/share/fonts/truetype/freefont/FreeMono.ttf";
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";
    "C:/Windows/Fonts/arial.ttf";
    "C:/Windows/Fonts/cour.ttf";
    "/Library/Fonts/Arial.ttf";
    "/System/Library/Fonts/SFNS.ttf";
    "font.ttf";
    "arial.ttf";
  ] in
  
  let rec try_paths = function
    | [] -> None
    | path::rest ->
      match Ttf.open_font path 16 with
      | Ok font -> Some font
      | Error _ -> try_paths rest
  in
  try_paths font_paths

let render_debug_text renderer font =
  let color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255 in
  let fps_text = Printf.sprintf "FPS: %d" !fps_counter in
  match Ttf.render_text_solid font fps_text color with
  | Error _ -> ()  
  | Ok surface -> 
    match Sdl.create_texture_from_surface renderer surface with
    | Error _ -> Sdl.free_surface surface
    | Ok texture -> 
      let (_, _, (w, h)) = match Sdl.query_texture texture with
        | Ok info -> info
        | Error _ -> (Sdl.Pixel.format_argb8888, Sdl.Texture.access_static, (100, 20))
      in
      let dst_rect = Sdl.Rect.create ~x:10 ~y:10 ~w ~h in
      let src_rect = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in
      ignore (Sdl.render_copy renderer texture ~src:src_rect ~dst:dst_rect);
      Sdl.free_surface surface;
      Sdl.destroy_texture texture

let tick (_t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.map (fun _ -> 0) prev in
  let width, height = Screen.dimensions s in
  let stars = generate_stars 100 width height |> twinkle in
  render s stars |> Framebuffer.render buffer;
  calculate_fps ();
  buffer

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "SDL init failed: %s" e; exit 1
  | Ok () -> 
    match Ttf.init () with
    | Error (`Msg e) -> Sdl.log "TTF init failed: %s" e; Sdl.quit (); exit 1
    | Ok () -> 
      let maybe_font = load_font () in
      match Sdl.create_window ~w:400 ~h:400 "Twinkling Stars" Sdl.Window.opengl with
      | Error (`Msg e) -> 
        Sdl.log "Window creation failed: %s" e;
        (match maybe_font with Some f -> Ttf.close_font f | None -> ());
        Ttf.quit (); Sdl.quit (); exit 1
      | Ok window ->
        match Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated with
        | Error (`Msg e) -> 
          Sdl.log "Renderer creation failed: %s" e;
          Sdl.destroy_window window;
          (match maybe_font with Some f -> Ttf.close_font f | None -> ());
          Ttf.quit (); Sdl.quit (); exit 1
        | Ok renderer ->
          let screen = Screen.create 400 400 1 (Palette.generate_mono_palette 16) in
          let tick_wrapper t s p i = 
            let buffer = tick t s p i in
            ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
            ignore (Sdl.render_clear renderer);
            (match maybe_font with
             | Some font -> render_debug_text renderer font
             | None -> ());
            ignore (Sdl.render_present renderer);
            buffer
          in
          Base.run "Twinkling Stars" None tick_wrapper screen;

          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          (match maybe_font with Some f -> Ttf.close_font f | None -> ());
          Ttf.quit ();
          Sdl.quit ()
