
open Extlib
open Sxp
open Parser

let () =
  let input = read_line ()
  in let read =
       match Parser.run_parser Reader.expr input with
       | None -> "<no read>"
       | Some (x, _) -> to_string x
     in print_endline read
