let () =
  Dsfmt.init_int(12345);
  for i = 1 to 100000000 do
    ignore (Dsfmt.genrand ());
  done;
