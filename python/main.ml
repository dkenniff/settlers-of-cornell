(** This file displays to the console the default Python interpreter for PyML *)

let _ =
  Py.initialize ();
  Py.Run.eval ~start:Py.File "import sys; print(sys.executable)"
