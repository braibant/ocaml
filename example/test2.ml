let max_young_wosize = 256

let string_major n = String.make (max_young_wosize * 8 + Random.int 256) (Char.chr (n mod 256))
let string_young n = String.make (128 + Random.int 256) (Char.chr (n mod 256))

let () =
  let control = Gc.get () in
  Gc.set (control.Gc.verbose <- 0x214; control)

let count = 100_000

let () =
  let long = ref [] in
  for _ = 0 to 10 do
    let r1 = ref [] in
    let r2 = ref [] in
    for i = 0 to count do
      r1 := string_major i :: !r1;
      r2 := string_major i :: !r2;
    done;
    r2 := [];
    for i = 0 to count do
      r2 := string_major i :: !r2;
      if i mod 1000 = 0 then long := string_major i :: !long;
    done;
  done
;;
