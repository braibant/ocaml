module IMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let message n = String.make (128 + Random.int 256) (Char.chr (n mod 256))

let window_size = 200_000
let msg_count = 1_000_000

let push_msg chan high_id =
  let low_id = high_id - window_size in
  let inserted = IMap.add high_id (message high_id) chan in
  if low_id < 0 then inserted
  else IMap.remove low_id inserted

let () =
  for _ = 0 to 10 do
    let r = ref IMap.empty in
    for i = 0 to msg_count do
      r := push_msg !r i
    done;
  done
;;
