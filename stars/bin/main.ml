open Claudius
open Tsdl


type star = {
  x : int;
  y : int;
  brightness : int;
}

let generate_stars num_stars width height =
  let rand_brightness () = 1 + Random.int 15 in
  List.init num_stars (fun _ ->
    { x = Random.int width; y = Random.int height; brightness = rand_brightness () })

let twinkle stars =
  List.map (fun star -> { star with brightness = max 1 ((star.brightness + (Random.int 3 - 1)) mod 16) }) stars

let render (_s : Screen.t) (stars : star list) : Primitives.t list =
  List.map (fun star ->
    Primitives.Pixel ({ x = star.x; y = star.y }, star.brightness)
  ) stars

let last_time = ref (Int32.of_int 0)
let fps = ref 0

(* Calculating FPS *)
let render_debug_info (s : Screen.t) () =
  let now = Sdl.get_ticks () in
  let elapsed = Int32.sub now !last_time in
  if elapsed > 01l then fps := 1000 / (Int32.to_int elapsed);  
  last_time := now;
  let width, height = Screen.dimensions s in
  Sdl.log "FPS: %d | Resolution: %dx%d" !fps width height

let tick (_t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.map (fun _ -> 0) prev in
  let width, height = Screen.dimensions s in
  let stars = generate_stars 100 width height |> twinkle in
  render s stars |> Framebuffer.render buffer;
  render_debug_info s ();
  buffer

let () =
  Screen.create 400 400 1 (Palette.generate_mono_palette 16) |> Base.run "Twinkling Stars" None tick
