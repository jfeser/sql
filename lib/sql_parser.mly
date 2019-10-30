/*
Simple SQL parser
*/


%{
    open Sql

    (* preserve order *)
    let make_limit l =
      let param = function
        | _, `Const _ -> None
        | x, `Param (None,pos) -> Some ((Some (match x with `Limit -> "limit" | `Offset -> "offset"),pos), Type.Int)
        | _, `Param p -> Some (p, Type.Int)
      in
      List.filter_map param l, List.mem (`Limit,`Const 1) l

    let mk_not x = Fun (`Not, [x])
%}

%token <int> INTEGER
%token <string> IDENT TEXT BLOB
%token <float> FLOAT
%token <Sql.param_id> PARAM
%token <int> LCURLY RCURLY
%token LPAREN RPAREN COMMA EOF DOT NULL CONFLICT_ALGO SELECT INSERT OR INTO
CREATE UPDATE VIEW TABLE VALUES WHERE ASTERISK DISTINCT ALL ANY SOME LIMIT ORDER
BY DESC ASC EQUAL DELETE FROM OFFSET SET JOIN LIKE_OP LIKE TILDE NOT BETWEEN AND
USING UNION EXCEPT INTERSECT AS TO CONCAT_OP JOIN_TYPE1 JOIN_TYPE2 NATURAL CROSS
REPLACE IN GROUP HAVING UNIQUE KEY ON IF EXISTS COLLATE CHARACTER DATE TIME
TIMESTAMP RENAME DROP INDEX CASE WHEN THEN ELSE DELAYED FOR SHARE MODE LOCK OF
WITH NOWAIT IS INTERVAL SUBSTRING DIV MOD LSH RSH BIT_AND BIT_OR GE GT LE LT EQ
NEQ MICROSECOND SECOND MINUTE HOUR DAY WEEK MONTH QUARTER YEAR
SECOND_MICROSECOND MINUTE_MICROSECOND MINUTE_SECOND HOUR_MICROSECOND HOUR_SECOND
HOUR_MINUTE DAY_MICROSECOND DAY_SECOND DAY_MINUTE DAY_HOUR YEAR_MONTH FALSE TRUE
DUPLICATE PLUS MINUS NOT_DISTINCT_OP COUNT SUM AVG MIN MAX


%left OR CONCAT_OP
%left AND
%nonassoc NOT
%nonassoc BETWEEN CASE (* WHEN THEN ELSE *) (* never useful *)
%nonassoc EQUAL EQ NEQ NOT_DISTINCT_OP IS LIKE LIKE_OP IN
%nonassoc GT LT GE LE
%left BIT_OR
%left BIT_AND
%left LSH RSH
%left PLUS MINUS
%left ASTERISK MOD DIV
(* ^ *)
%nonassoc UNARY_MINUS TILDE
(* Warning: the precedence level assigned to BINARY is never useful. *)
(* %nonassoc BINARY COLLATE *)
%nonassoc INTERVAL

%type <Sql.op Sql.expr> expr

%start <Sql.op Sql.stmt> input

%%

input: statement EOF { $1 }

if_not_exists: IF NOT EXISTS { }
if_exists: IF EXISTS {}

statement: 
  | CREATE either(TABLE,VIEW) name=IDENT AS select=maybe_parenth(select_stmt)
    {
      Create (name,`Select select)
    }
  | RENAME TABLE l=separated_nonempty_list(COMMA, separated_pair(IDENT,TO,IDENT)) { Rename l }
  | DROP either(TABLE,VIEW) if_exists? name=IDENT
    {
      Drop name
    }
  | CREATE UNIQUE? INDEX if_not_exists? name=table_name ON table=table_name cols=sequence(index_column)
    {
      CreateIndex (name, table, cols)
    }
  | select_stmt { Select $1 }
  | insert_cmd target=IDENT names=sequence(IDENT)? VALUES values=commas(sequence(expr))? ss=on_duplicate?
    {
      Insert { target; action=`Values (names, values); on_duplicate=ss; }
    }
  | insert_cmd target=IDENT names=sequence(IDENT)? select=maybe_parenth(select_stmt) ss=on_duplicate?
    {
      Insert { target; action=`Select (names, select); on_duplicate=ss; }
    }
  | insert_cmd target=IDENT SET set=commas(set_column)? ss=on_duplicate?
    {
      Insert { target; action=`Set set; on_duplicate=ss; }
    }
  | update_cmd table=IDENT SET ss=commas(set_column) w=where? o=loption(order) lim=loption(limit)
    {
      Update (table,ss,w,o,lim)
    }
/* http://dev.mysql.com/doc/refman/5.1/en/update.html multi-table syntax */
  | update_cmd tables=commas(source) SET ss=commas(set_column) w=where?
    {
      UpdateMulti (tables,ss,w)
    }
  | DELETE FROM table=IDENT w=where?
    {
      Delete (table,w)
    }
  | SET name=IDENT EQUAL e=expr
    {
      Set (name, e)
    }

table_name: name=IDENT | IDENT DOT name=IDENT { name } (* FIXME db name *)
index_prefix: LPAREN n=INTEGER RPAREN { n }
index_column: name=IDENT index_prefix? collate? order_type? { name }

(* ugly, can you fixme? *)
(* ignoring everything after RPAREN (NB one look-ahead token) *)

select_stmt: select_core other=list(preceded(compound_op,select_core)) o=loption(order) lim=limit_t? select_row_locking?
    {
      { select = ($1, other); order=o; limit=lim; }
    }

select_core: SELECT select_type? r=commas(column1) f=from?  w=where?  g=loption(group) h=having?
    {
      { columns=r; from=f; where=w; group=g; having=h; }
    }

table_list: src=source joins=join_source* { (src,joins) }

join_source: NATURAL maybe_join_type JOIN src=source { src,`Natural }
  | CROSS JOIN src=source { src,`Cross }
  | qualified_join src=source cond=join_cond { src,cond }

qualified_join: COMMA | maybe_join_type JOIN { }

join_cond: ON e=expr { `Search e }
  | USING l=sequence(IDENT) { `Using l }
  | (* *) { `Default }

source1: IDENT { `Table $1 }
  | LPAREN s=select_stmt RPAREN { `Select s }
  | LPAREN s=table_list RPAREN { `Nested s }

source: src=source1 alias=maybe_as { src, alias }

insert_cmd: INSERT DELAYED? OR? conflict_algo INTO | INSERT INTO | REPLACE INTO { }
update_cmd: UPDATE | UPDATE OR conflict_algo { }
conflict_algo: CONFLICT_ALGO | REPLACE { }
on_duplicate: ON DUPLICATE KEY UPDATE ss=commas(set_column) { ss }

select_type: DISTINCT | ALL { }

select_row_locking:
for_update_or_share+
    { }
  | LOCK IN SHARE MODE
    { }

for_update_or_share:
FOR either(UPDATE, SHARE) update_or_share_of? NOWAIT? with_lock? { }

update_or_share_of: OF commas(IDENT) { }

with_lock: WITH LOCK { }

int_or_param: i=INTEGER { `Const i }
  | p=PARAM { `Param p }

limit_t: LIMIT lim=int_or_param { make_limit [`Limit,lim] }
  | LIMIT ofs=int_or_param COMMA lim=int_or_param { make_limit [`Offset,ofs; `Limit,lim] }
  | LIMIT lim=int_or_param OFFSET ofs=int_or_param { make_limit [`Limit,lim; `Offset,ofs] }

limit: limit_t { fst $1 }

order: ORDER BY l=commas(pair(expr,order_type?)) { l }
order_type:
  | DESC { `Desc }
  | ASC { `Asc }

from: FROM t=table_list { t }
where: WHERE e=expr { e }
group: GROUP BY l=expr_list { l }
having: HAVING e=expr { e }

column1:
  | IDENT DOT ASTERISK { Sql.AllOf $1 }
  | ASTERISK { Sql.All }
  | e=expr m=maybe_as { Sql.Expr (e,m) }

maybe_as: AS? name=IDENT { Some name }
  | { None }

maybe_parenth(X): x=X | LPAREN x=X RPAREN { x }

set_column: name=attr_name EQUAL e=expr { name,e }

anyall: ANY | ALL | SOME { }

mnot(X): NOT x = X | x = X { x }

attr_name: cname=IDENT { { cname; tname=None} }
  | table=IDENT DOT cname=IDENT
  | IDENT DOT table=IDENT DOT cname=IDENT { {cname; tname=Some table} } (* FIXME database identifier *)

like_expr:
  | e1=expr; like; e2=expr %prec LIKE { Fun (`Like, [e1;e2]) }
  | e1=expr; NOT; like; e2=expr %prec LIKE { Fun (`Not, [Fun (`Like, [e1;e2])]) }

expr:
  | expr numeric_bin_op expr %prec PLUS { Fun ($2, [$1;$3]) }
  | expr boolean_bin_op expr %prec AND { Fun ($2, [$1;$3]) }
  | e1=expr; op=comparison_op; anyall?; e2=expr %prec EQUAL { Fun(op, [e1;e2]) }
  | expr CONCAT_OP expr { Fun (`Concat,[$1;$3]) }
  | e=like_expr { e }
  | op=unary_op; e=expr { Fun(op, [e]) }
  | MINUS expr %prec UNARY_MINUS { $2 }
  | INTERVAL e=expr i=interval_unit { Fun (`Interval i, [e]) }
  | LPAREN expr RPAREN { $2 }
  | attr_name collate? { Column $1 }
  | VALUES LPAREN n=IDENT RPAREN { Inserted n }
  | v=literal_value { Value v }
  | e1=expr IN l=sequence(expr) { Fun (`In, [e1; Sequence l]) }
  | e1=expr NOT IN l=sequence(expr) { mk_not @@ Fun (`In, [e1; Sequence l]) }
  | e1=expr IN LPAREN select=select_stmt RPAREN { Fun (`In, [e1; Select (select, `AsValue)]) }
  | e1=expr NOT IN LPAREN select=select_stmt RPAREN { mk_not @@ Fun (`In, [e1; Select (select, `AsValue)]) }
  | e1=expr IN IDENT { e1 }
  | LPAREN select=select_stmt RPAREN { Select (select, `AsValue) }
  | PARAM { Param ($1,Any) }
  | p=PARAM LCURLY l=choices c2=RCURLY { let (name,(p1,_p2)) = p in Choices ((name,(p1,c2+1)),l) }
  | SUBSTRING LPAREN s=expr FROM p=expr FOR n=expr RPAREN
  | SUBSTRING LPAREN s=expr COMMA p=expr COMMA n=expr RPAREN { Fun (`Substring, [s;p;n]) }
  | SUBSTRING LPAREN s=expr either(FROM,COMMA) p=expr RPAREN { Fun (`Substring, [s;p]) }
  | f=IDENT LPAREN p=func_params RPAREN { Fun (`Call f, p) }
  | expr IS NULL { Fun (`IsNull, [$1]) }
  | expr IS NOT NULL { mk_not @@ Fun (`IsNull, [$1]) }
  | e1=expr IS e2=expr { Fun (`Is, [e1;e2]) }
  | e1=expr IS NOT e2=expr { mk_not @@ Fun (`Is, [e1;e2]) }
  | e1=expr IS DISTINCT FROM e2=expr { Fun (`IsDistinct, [e1;e2]) }
  | e1=expr IS NOT DISTINCT FROM e2=expr { mk_not @@ Fun (`IsDistinct, [e1;e2]) }
  | expr BETWEEN expr AND expr { Fun(`Between, [$1;$3;$5]) }
  | e1=expr; NOT; BETWEEN; e2=expr; AND; e3=expr { Fun (`Not, [Fun(`Between, [e1;e2;e3])]) }
  | EXISTS; LPAREN select=select_stmt RPAREN { Select (select,`Exists) }
  | NOT; EXISTS; LPAREN select=select_stmt RPAREN { Fun (`Not, [Select (select, `Exists)]) }
  | COUNT LPAREN e = expr RPAREN { Fun (`Count, [e]) }
  | COUNT LPAREN ASTERISK RPAREN { Fun (`Count, []) }
  | MIN LPAREN e = expr RPAREN { Fun (`Min, [e]) }
  | MAX LPAREN e = expr RPAREN { Fun (`Max, [e]) }
  | SUM LPAREN e = expr RPAREN { Fun (`Sum, [e]) }
  | AVG LPAREN e = expr RPAREN { Fun (`Avg, [e]) }
  /* | CASE e1=expr? branches=nonempty_list(case_branch) e2=preceded(ELSE,expr)? END (* FIXME typing *) */
  /*   { */
  /*     let maybe f = function None -> [] | Some x -> [f x] in */
  /*     let t_args = */
  /*       match e1 with */
  /*       | None -> (List.flatten @@ List.map (fun _ -> [Typ Bool; Var 1]) branches) */
  /*       | Some _ -> [Var 0] @ (List.flatten @@ List.map (fun _ -> [Var 0; Var 1]) branches) */
  /*     in */
  /*     let t_args = t_args @ maybe (fun _ -> Var 1) e2 in */
  /*     let v_args = maybe Prelude.identity e1 @ List.flatten branches @ maybe Prelude.identity e2 in */
  /*     Fun (F (Var 1, t_args), v_args) */
  /*   } */
  | IF LPAREN e1=expr COMMA e2=expr COMMA e3=expr RPAREN { Fun (`Ite, [e1;e2;e3]) }

case_branch: WHEN e1=expr THEN e2=expr { [e1;e2] }
like: LIKE | LIKE_OP { }

choice_body: c1=LCURLY e=expr c2=RCURLY { (c1,Some e,c2) }
choice: name=IDENT? e=choice_body?
    {
      let (c1,e,c2) = Option.value ~default:(0,None,0) e in ((name, (c1+1,c2)),e)
    }
choices: separated_nonempty_list(BIT_OR,choice) { $1 }

literal_value:
  | x = TEXT collate? { String x }
  | BLOB collate? { failwith "BLOB not supported" }
  | x = INTEGER { Int x }
  | x = FLOAT { Float x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | DATE; x = TEXT
  | TIME; x = TEXT
  | TIMESTAMP; x = TEXT { Date x }
  | NULL { Null }

single_literal_value:
  | literal_value { $1 }
  | MINUS; x = INTEGER { Int (~-x) }
  | MINUS; x = FLOAT { Float (~-.x) }

expr_list: l=commas(expr) { l }
func_params: DISTINCT? l=expr_list { l }
  | ASTERISK { [] }
  | (* *) { [] }
numeric_bin_op:
  | PLUS { `Add }
  | MINUS { `Sub }
  | ASTERISK { `Mul }
  | DIV { `Div }
  | MOD { `Mod }
  | LSH { `Lsh }
  | RSH { `Rsh }
  | BIT_OR { `Bit_or }
  | BIT_AND { `Bit_and }

comparison_op:
  | EQUAL | EQ {`Eq}
  | GT { `Gt }
  | GE { `Ge }
  | LT { `Lt }
  | LE { `Le }
  | NEQ { `Neq }
  | NOT_DISTINCT_OP { `NotDistinct }

boolean_bin_op:
  | AND {`And}
  | OR {`Or}

unary_op:
  | TILDE { `Bit_not }
  | NOT { `Not }

interval_unit: MICROSECOND | SECOND | MINUTE | HOUR | DAY | WEEK | MONTH | QUARTER | YEAR
  | SECOND_MICROSECOND | MINUTE_MICROSECOND | MINUTE_SECOND
  | HOUR_MICROSECOND | HOUR_SECOND | HOUR_MINUTE
  | DAY_MICROSECOND | DAY_SECOND | DAY_MINUTE | DAY_HOUR
  | YEAR_MONTH { }

%inline either(X,Y): X | Y { }
%inline commas(X): l=separated_nonempty_list(COMMA,X) { l }
(* (x1,x2,...,xn) *)
%inline sequence_(X): LPAREN l=commas(X) { l }
%inline sequence(X): l=sequence_(X) RPAREN { l }

collate: COLLATE IDENT { }

compound_op: UNION ALL? | EXCEPT | INTERSECT { }

maybe_join_type: JOIN_TYPE1? JOIN_TYPE2? { }

