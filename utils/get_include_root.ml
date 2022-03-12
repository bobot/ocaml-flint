let () =
  print_endline
    (Filename.concat (Sys.getcwd ())
       (Filename.dirname (Filename.dirname (Filename.dirname Sys.argv.(1)))))
