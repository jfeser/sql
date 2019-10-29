exception Error of exn * (int * int * string * string)

let parse_buf_exn lexbuf =
  try Sql_parser.input Sql_lexer.parse_rule lexbuf
  with exn ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    let tail = Sql_lexer.ruleTail "" lexbuf in
    raise (Error (exn, (line, cnum, tok, tail)))

let parse_buf lexbuf = try Some (parse_buf_exn lexbuf) with _ -> None

let parse_stdin () = parse_buf (Lexing.from_channel stdin)

let parse_string str =
  (*Error.log "Parsing : %s" str; *)
  parse_buf (Lexing.from_string str)

let parse_file filename =
  let contents = try Std.input_file filename with _ -> "" in
  parse_string contents

let parse_stmt stmt = parse_buf_exn (Lexing.from_string stmt)
