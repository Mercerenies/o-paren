
open Extlib
open Sxp

let () =
  let input = read_line ()
  in let read =
       match Reader.read_expr input with
       | None -> "<no read>"
       | Some (x, _) -> to_string x
     in print_endline read
