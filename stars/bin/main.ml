open Claudius

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

let tick (_t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.map (fun _ -> 0) prev in
  let width, height = Screen.dimensions s in
  let stars = generate_stars 100 width height |> twinkle in
  render s stars |> Framebuffer.render buffer;
  buffer

let () =
  Screen.create 640 480 1 (Palette.generate_mono_palette 16) |> Base.run "Twinkling Stars" None tick