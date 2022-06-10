let () =
  let r = ref Sys.argv.(2) in
  for i = 1 to int_of_string Sys.argv.(1) do
    r := Filename.dirname !r
  done;
  let r = !r in
  let r =
    if Filename.is_relative r then Filename.concat (Sys.getcwd ()) r else r
  in
  print_string r
