let () =
  Random.init(12345);
  for i = 1 to 100000000 do
    ignore (Random.float (1.0));
  done;
