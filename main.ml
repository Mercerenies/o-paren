
open Extlib
open Result

let read = Sxp.read_expr
let eval = Eval.eval
let print x = print_endline (Sxp.to_string x)

let rec loop env () =
  print_string ">> ";
  let input = read_line ()
  in let () = match read input with
       | None -> print_endline "<no read>"
       | Some x -> match eval env x with
                   | Error e -> print_endline ("<Error: " ^ e ^ ">")
                   | Success x -> print x
     in loop env ()

let () = loop (Built_ins.construct_env ()) ()
