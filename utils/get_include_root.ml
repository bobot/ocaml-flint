let () =
  let r = ref Sys.argv.(2) in
  for i = 1 to int_of_string Sys.argv.(1) do
    r := Filename.dirname !r
  done;
  print_string (Filename.concat (Sys.getcwd ()) !r)
