{
open Lexing

module Make (L : Location.Locator) = struct
module P = Parser.Make (L)
open P

}


let int = ['1'-'9'] ['0'-'9']* | '0'


let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?


let white = [' ' '\t' '\n' '\r']+
let comment = '(' '*' ( [^ '*']* | '*' [^ ')']) * '*' ')'
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
  parse
  | white    { read lexbuf }
  | comment  { read lexbuf }
  | "->"     { ARROW }
  | "("      { LPAR }
  | ")"      { RPAR }
  | "{"      { LBRACE }
  | "}"      { RBRACE }
  | "["      { LBRACK }
  | "]"      { RBRACK }
  | ";"      { SEMI }
  | "."      { DOT }
  | "|"      { TY_JOIN }
  | "&"      { TY_MEET }
  | ":"      { ASC }
  | "<:"     { SUBSUME }
  | "Top"    { TOP }
  | "Bot"    { BOT }
  | "rec"    { REC }
  | "="      { EQUALS }
  | "fun"    { FUN }
  | "unit"   { UNIT }
  | "let"    { LET }
  | "in"     { IN }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "check"  { CHECK }

  | "=="     { EQEQUALS }
  | "<"      { CMP_LT }
  | ">"      { CMP_GT }
  | "<="     { CMP_LTE }
  | ">="     { CMP_GTE }
  | "+"      { OP_ADD }
  | "-"      { OP_SUB }

  | "list"   { LIST }
  | "::"     { CONS }
  | "match"  { MATCH }
  | "with"   { WITH }

  | id       { IDENT (Symbol.intern (Lexing.lexeme lexbuf)) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }

{
end
}
