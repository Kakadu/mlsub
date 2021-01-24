open Lexer
open Lexing
open Printf
open Types
open Typecheck

let run exp =
  let name, chan = Filename.open_temp_file ~mode:[ Open_wronly ] "tmp" "ml" in
  Camlgen.to_caml (Format.formatter_of_out_channel chan) exp;
  close_out chan;
  ignore (Sys.command ("cat " ^ name ^ "; ocaml " ^ name));
  Sys.remove name

let reparse s = compile_terms (fun f -> f Pos (decompile_automaton s))

let recomp s =
  decompile_automaton (compile_terms (fun f -> f Pos (decompile_automaton s)))

let process file =
  let print_err e = Types.Reason.print Format.err_formatter e in
  let check gamma = function
    | `Exp (name, exp) -> Format.printf "%a\n" Exp.pp_exp exp
    | `Subs (t1, t2) -> failwith "not impelemented"
  in

  try
    ignore
      (List.fold_left check Typecheck.gamma0
         (Source.parse_modlist (Location.of_file file)))
  with
  | Failure msg -> Format.printf "Typechecking failed: %s\n%!" msg
  | Match_failure (file, line, col) ->
      Format.printf "Match failure in typechecker at %s:%d%d\n%!" file line col

let () =
  Format.set_max_indent 0;
  Format.set_margin 120;
  if Array.length Sys.argv = 1 then
    Printf.fprintf stderr "Usage: %s <input file>\n" Sys.argv.(0) (* repl () *)
  else Array.iter process (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
