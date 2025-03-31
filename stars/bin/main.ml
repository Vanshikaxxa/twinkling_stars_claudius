type star = {
  x : int;
  y : int;
  brightness : int;
}

let stars = ref []
let num_stars = 100
let last_width = ref 0
let last_height = ref 0

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
let last_time = ref (0.0)
let frame_count = ref 0
let fps_counter = ref 0

let calculate_fps () =
  let now = Unix.gettimeofday () in
  let elapsed = now -. !last_time in
  frame_count := !frame_count + 1;
  if elapsed >= 1.0 then (
    fps_counter := !frame_count;
    frame_count := 0;
    last_time := now
  )

let render (_stars : star list) (show_stats : bool) (width : int) (height : int) : Primitives.t list =
  let star_primitives = 
    List.map (fun star -> Primitives.Pixel ({ x = star.x; y = star.y }, star.brightness)) _stars
  in
  if show_stats then
    let fps_text = Printf.sprintf "FPS: %d" !fps_counter in
    let dots_text = Printf.sprintf "DOTS: %d" (List.length _stars) in
    let res_text = Printf.sprintf "RES: %dx%d" width height in
    let text_primitives = 
      let base_x = 2 in
      let base_y = 2 in
      let draw_char x y c acc = 

        let draw_pixel px py b acc =
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
        | 'D' ->
          let pixels = [
            (x, y); (x+1, y); (x+2, y);
            (x, y+1); (x+3, y+1);
            (x, y+2); (x+3, y+2);
            (x, y+3); (x+3, y+3);
            (x, y+4); (x+1, y+4); (x+2, y+4)
          ] in
          List.fold_left (fun acc (px, py) -> 
            draw_pixel (px*2) (py*2) 15 acc
          ) acc pixels
        | 'O' ->
          let pixels = [
            (x+1, y); (x+2, y);
            (x, y+1); (x+3, y+1);
            (x, y+2); (x+3, y+2);
            (x, y+3); (x+3, y+3);
            (x+1, y+4); (x+2, y+4)
          ] in
          List.fold_left (fun acc (px, py) -> 
            draw_pixel (px*2) (py*2) 15 acc
          ) acc pixels
        | 'T' ->
          let pixels = [
            (x, y); (x+1, y); (x+2, y); (x+3, y);
            (x+1, y+1); (x+2, y+1);
            (x+1, y+2); (x+2, y+2);
            (x+1, y+3); (x+2, y+3);
            (x+1, y+4); (x+2, y+4)
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
        | 'R' ->
          let pixels = [
            (x, y); (x+1, y); (x+2, y);
            (x, y+1); (x+3, y+1);
            (x, y+2); (x+1, y+2); (x+2, y+2);
            (x, y+3); (x+2, y+3);
            (x, y+4); (x+3, y+4)
          ] in
          List.fold_left (fun acc (px, py) -> 
            draw_pixel (px*2) (py*2) 15 acc
          ) acc pixels
        | 'E' ->
          let pixels = [
            (x, y); (x+1, y); (x+2, y); (x+3, y);
            (x, y+1);
            (x, y+2); (x+1, y+2); (x+2, y+2);
            (x, y+3);
            (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
          ] in
          List.fold_left (fun acc (px, py) -> 
            draw_pixel (px*2) (py*2) 15 acc
          ) acc pixels
        | 'x' ->
          let pixels = [
            (x, y); (x+3, y);
            (x+1, y+1); (x+2, y+1);
            (x+1, y+2); (x+2, y+2);
            (x+1, y+3); (x+2, y+3);
            (x, y+4); (x+3, y+4)
          ] in
          List.fold_left (fun acc (px, py) -> 
            draw_pixel (px*2) (py*2) 15 acc
          ) acc pixels
        | '0'..'9' as digit ->
          let n = Char.code digit - Char.code '0' in
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
        | _ -> acc 
      in
      let rec draw_string x y chars acc =
        match chars with
        | [] -> acc
        | c :: cs -> 
          let new_acc = draw_char (x/2) (y/2) c acc in
          draw_string (x + 10) y cs new_acc 
      in
      let fps_primitives = draw_string (base_x*2) (base_y*2) (List.of_seq (String.to_seq fps_text)) [] in
      let dots_primitives = draw_string (base_x*2) ((base_y + 14)*2) (List.of_seq (String.to_seq dots_text)) [] in
      let res_primitives = draw_string (base_x*2) ((base_y + 28)*2) (List.of_seq (String.to_seq res_text)) [] in
      fps_primitives @ dots_primitives @ res_primitives
    in
    star_primitives @ text_primitives
  else
    star_primitives

let f_key = Key.F

let tick (_t : int) (s : Screen.t) (prev : Framebuffer.t) (inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.map (fun _ -> 0) prev in
  let width, height = Screen.dimensions s in
  
  let size_changed = !last_width != width || !last_height != height in
  
  last_width := width;
  last_height := height;
  
  if size_changed || !stars = [] || List.length !stars != num_stars then (
    stars := generate_stars num_stars width height
  ) else (
    stars := twinkle !stars;

    stars := List.map (fun star ->
      { star with
        x = min (max star.x 0) (width - 1);
        y = min (max star.y 0) (height - 1)
      }
    ) !stars
  );
  
  calculate_fps ();
  
  let show_stats = Base.KeyCodeSet.mem f_key inputs in
  
  render !stars show_stats width height |> Framebuffer.render buffer;
  buffer

let () =
  Random.self_init ();
  last_width := 400;
  last_height := 400;
  let screen = Screen.create ~resizable:true 400 400 1 (Palette.generate_mono_palette 16) in
  Base.run "Twinkling Stars" None tick screen
