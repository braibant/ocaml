open Printf

module P = struct
  let jump_samples = Queue.create ()
  let alloc_samples = Queue.create ()

  let jumps = Array.make 20 0
  let allocs = Array.make 20 0
  let largest_jump = ref min_int
  let largest_alloc = ref min_int

  let set_jump i value = largest_jump := max !largest_jump value; jumps.(i) <- value
  let set_alloc i value = largest_alloc := max !largest_alloc value;  allocs.(i) <- value

  let read_log_line line =
    Scanf.sscanf line "@@ %19d %19d %s@\n"
      (fun start value name ->
         match name with
         | "fljump00@"     ->  set_jump   0 value
         | "fljump01@"     ->  set_jump   1 value
         | "fljump02@"     ->  set_jump   2 value
         | "fljump03@"     ->  set_jump   3 value
         | "fljump04@"     ->  set_jump   4 value
         | "fljump05@"     ->  set_jump   5 value
         | "fljump06@"     ->  set_jump   6 value
         | "fljump07@"     ->  set_jump   7 value
         | "fljump08@"     ->  set_jump   8 value
         | "fljump09@"     ->  set_jump   9 value
         | "fljump10-19@"  ->  set_jump  10 value
         | "fljump20-29@"  ->  set_jump  11 value
         | "fljump30-39@"  ->  set_jump  12 value
         | "fljump40-49@"  ->  set_jump  13 value
         | "fljump50-59@"  ->  set_jump  14 value
         | "fljump60-69@"  ->  set_jump  15 value
         | "fljump70-79@"  ->  set_jump  16 value
         | "fljump80-89@"  ->  set_jump  17 value
         | "fljump90-99@"  ->  set_jump  18 value
         | "fljump_large@" ->  set_jump  19 value; Queue.add  (Array.copy jumps) jump_samples

         | "alloc00@"     ->  set_alloc   0 value
         | "alloc01@"     ->  set_alloc   1 value
         | "alloc02@"     ->  set_alloc   2 value
         | "alloc03@"     ->  set_alloc   3 value
         | "alloc04@"     ->  set_alloc   4 value
         | "alloc05@"     ->  set_alloc   5 value
         | "alloc06@"     ->  set_alloc   6 value
         | "alloc07@"     ->  set_alloc   7 value
         | "alloc08@"     ->  set_alloc   8 value
         | "alloc09@"     ->  set_alloc   9 value
         | "alloc10-19@"  ->  set_alloc  10 value
         | "alloc20-29@"  ->  set_alloc  11 value
         | "alloc30-39@"  ->  set_alloc  12 value
         | "alloc40-49@"  ->  set_alloc  13 value
         | "alloc50-59@"  ->  set_alloc  14 value
         | "alloc60-69@"  ->  set_alloc  15 value
         | "alloc70-79@"  ->  set_alloc  16 value
         | "alloc80-89@"  ->  set_alloc  17 value
         | "alloc90-99@"  ->  set_alloc  18 value
         | "alloc_large@" ->  set_alloc  19 value; Queue.add  (Array.copy allocs) alloc_samples
         | _ -> ()
      )
  ;;


end

module Svg = struct

  let filled_rect ~x ~y ~width ~height ~style ~onmouseover ~onmouseout =
    sprintf
      {|<rect x="%i" y="%i" width="%i" height="%i" style="%s" onmouseover="%s" onmouseout="%s" />|}
      x y width height style onmouseover onmouseout

  let string ~x ~y ~str ~extra =
    sprintf {| <text x="%i" y="%i" %s> %s </text> |} x y extra str

end

let boxsize = 10;;

let rgb value largest =
  if value = 0
  then 255,255,255
  else
    let ratio = float value /. largest in
    let r = 255 in
    let g = 240 - (int_of_float (240. *. ratio)) in
    let b = 220 - (int_of_float (220. *. ratio)) in
    r,g,b
;;

let heatmap queue largest =
  let buffer = Buffer.create 255 in
  if Queue.is_empty queue
  then ("")
  else
    begin
      let hd = Queue.peek queue in
      let length = Queue.length queue in
      let height = Array.length hd in
      for i = 0 to length - 1 do
        let current = Queue.pop queue in
        let x = i * boxsize in
        let acc = ref 0 in
        let total = Array.fold_left (+) 0 current in
        for j = 0 to height - 1 do
          let y = j * boxsize in
          let r,g,b = rgb current.(j) largest in
          let c = current.(j) in
          acc := !acc + c;
          Buffer.add_string buffer
            (Svg.filled_rect ~x ~y ~width:boxsize ~height:boxsize
               ~style:(sprintf "fill:rgb(%i,%i,%i)" r g b)
               ~onmouseover:(sprintf "s(%i,%i,%i)" c !acc total)
               ~onmouseout:(sprintf "c()")
            );
          Buffer.add_string buffer "\n"
        done;
      done;
      Buffer.add_string buffer
        (Svg.string  ~x:10 ~y:(height * boxsize - 10)
           ~extra:{|id="details" style="font-size:8"|} ~str:"");
      sprintf {|<svg width="%i" height="%i">%s</svg>|}
        (length * boxsize)
        (height * boxsize)
        (Buffer.contents buffer)
    end
;;

let script =
  {|<script type="text/ecmascript">
	  var details;
	  function init(evt) { details = document.getElementById("details").firstChild; }
	  function s(c, acc, total) {
		  var pct = Math.floor(c / total * 100);
		  var apct = Math.floor(acc / total * 100);
  		details.nodeValue = "count: " + c + ", pct: " + pct + "%, acc: " + acc + ", acc pct: " + apct + "%";
	}
	function c() { details.nodeValue = ' '; }
    </script>
  |}
;;

let events_of_file file =
  let ic = open_in file in
  ignore (input_line ic);
  let () =
    try
      while true do
        P.read_log_line (input_line ic);
      done;
      assert false
    with End_of_file ->
      ()
  in

  Printf.printf {|<!DOCTYPE html>
                    <html>
    %s
                    <body onload="init()">
    <h1>Jumps</h1>
                    %s
    <h1>Alloc</h1>
                    %s
                    </body>
                    </html>
                |}
    script
    (heatmap P.jump_samples (float !P.largest_jump))
    (heatmap P.alloc_samples (float !P.largest_alloc))
;;

let () = events_of_file Sys.argv.(1)
