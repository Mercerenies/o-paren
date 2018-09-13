
open Extlib
open Sxp
open Parser
open Eval
open Built_ins
open Cat

let () =
  let input = read_line ()
  in let read =
       match Sxp.read_expr input with
       | None -> "<no read>"
       | Some x -> to_string x
     in print_endline read
