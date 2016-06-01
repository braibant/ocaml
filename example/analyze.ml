let table = Array.make 20 []

let largest_count = ref min_int

let read_log_line line =
  Scanf.sscanf line "@@ %19d %19d %s@\n"
    (fun start value name ->
       let i =
         match name with
         | "fljump00@"     ->  Some  0
         | "fljump01@"     ->  Some  1
         | "fljump02@"     ->  Some  2
         | "fljump03@"     ->  Some  3
         | "fljump04@"     ->  Some  4
         | "fljump05@"     ->  Some  5
         | "fljump06@"     ->  Some  6
         | "fljump07@"     ->  Some  7
         | "fljump08@"     ->  Some  8
         | "fljump09@"     ->  Some  9
         | "fljump10-19@"  ->  Some 10
         | "fljump20-29@"  ->  Some 11
         | "fljump30-39@"  ->  Some 12
         | "fljump40-49@"  ->  Some 13
         | "fljump50-59@"  ->  Some 14
         | "fljump60-69@"  ->  Some 15
         | "fljump70-79@"  ->  Some 16
         | "fljump80-89@"  ->  Some 17
         | "fljump90-99@"  ->  Some 18
         | "fljump_large@" ->  Some 19
         | _ -> None
       in
       match i with
       | None -> ()
       | Some i ->
           largest_count := max !largest_count value;
           table.(i) <- value :: table.(i)
    )
;;

let current () =
  Array.map List.hd table

let next () =
  for i = 0 to Array.length table - 1 do
    table.(i) <- List.tl table.(i)
  done

let reverse () =
  for i = 0 to Array.length table - 1 do
    table.(i) <- List.rev table.(i)
  done

let is_empty () =
  table.(0) = []

module Svg = struct
  open Printf

  let filled_rect ~x ~y ~width ~height ~style =
    sprintf {|<rect x="%i" y="%i" width="%i" height="%i" style="%s"/>|}
      x y width height style
  ;;

end

let events_of_file file =
  let ic = open_in file in
  ignore (input_line ic);
  let () =
    try
      while true do
        read_log_line (input_line ic);
      done;
      assert false
    with End_of_file ->
      reverse ()
  in
  let largest = float !largest_count in
  let count = List.length table.(0) in
  let boxsize = 10 in
  let r = ref 0 in
  let buffer = Buffer.create 255 in
  while not (is_empty ()) do
    let current = current () in
    let x = !r * boxsize in
    for i = 0 to Array.length current - 1 do
      let y = i * boxsize in
      let r,g,b =
        if current.(i) = 0
        then 255,255,255
        else
          let ratio = float current.(i) /. largest in
          let r = 255 in
          let g = 240 - (int_of_float (240. *. ratio)) in
          let b = 220 - (int_of_float (220. *. ratio)) in
          r,g,b
      in
      Buffer.add_string buffer
        (Svg.filled_rect ~x ~y ~width:boxsize ~height:boxsize
           ~style:(Printf.sprintf "fill:rgb(%i,%i,%i)" r g b));
      Buffer.add_string buffer "\n"
    done;
    next ();
    incr r
  done;
  Printf.printf {|<!DOCTYPE html>
                  <html>
                  <body>
                  <svg width="%i" height="%i">%s</svg>
                  </body>
                  </html>
                |}
    (count * boxsize)
    (Array.length table * boxsize)
    (Buffer.contents buffer)
;;

let () = events_of_file Sys.argv.(1)
