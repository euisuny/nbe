(* Top-level read-eval-print loop. *)
exception Quit

open Debruijn

(* Parse an input expression. *)
let parse (s : string) : tm =
  Parser.parse Lexer.token (Lexing.from_string s)

let eval (e : tm) : unit =
  let tm = nf [] e in
  to_string tm |> print_endline

(* Read lines from the console, appending them to s, until the user enters a
 * blank line. *)
let read_console() : string =
  let rec read_lines (s : string) : string =
    let input = read_line() in
    if input = "" then s
    else read_lines (s ^ input ^ " ")
  in read_lines ""

(* Main read-eval-print loop. *)
let rec repl () : unit =
  print_string "? ";
  (try
    let input = read_console() in
    if input = "" then ()
    else
      let expr = parse input in
      ignore (eval expr)
  with Failure s -> print_endline ("Error: " ^ s)
  | Parsing.Parse_error -> print_endline "Parse Error"
  (* IY: What is the name of this parsing error? *)
  | _ -> print_endline "Parse Error"); repl ()

let main () =
  print_endline "This program computes the translation and the step by step evaluation of an expression. Type in an expression, then type <Enter> twice.";
  try repl()
  with End_of_file -> ()
