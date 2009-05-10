(* SQL syntax and RA *)

open Stmt
open Operators
open ListMore
open Apply

type expr = [ `Value of Sql.Type.t (** literal value *)
            | `Param of param
            | `Func of Sql.Type.t option * expr list (** return type, parameters *)
            | `Column of string * string option (** name, table *)
            ]
            deriving (Show)

type expr_q = [ `Value of Sql.Type.t (** literal value *)
            | `Param of param
            | `Func of Sql.Type.t option * expr_q list (** return type, parameters *)
            ]
            deriving (Show)

let expr_to_string = Show.show<expr>

type column =
  | All
  | AllOf of string
  | Expr of expr * string option (** name *)
  deriving (Show)

type columns = column list deriving (Show)

let collect f l = List.flatten (List.map f l)

let scheme_as_params = List.map (fun attr -> Named attr.RA.name, Some attr.RA.domain)

(** replace every Column with Value of corresponding type *)
let resolve_columns tables expr =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let rec each e =
    match e with
    | `Value x -> `Value x
    | `Column (name,x) ->
      let attr = RA.Scheme.find (Option.map_default scheme all x) name in
      `Value attr.RA.domain
    | `Param x -> `Param x
    | `Func (r,l) -> `Func (r,(List.map each l))
  in
  each expr

(** assign types to parameters where possible *)
let assign_types expr =
  let rec typeof e = (* FIXME simplify *)
    match e with
    | `Value t -> e, Some t
    | `Func (ret,l) ->
(** Assumption: sql functions/operators have type scheme 'a -> 'a -> 'b
    i.e. all parameters of some equal type *)
        let (l,t) = l >> List.map typeof >> List.split in
        let t = match List.filter_valid t with
        | [] -> None
        | h::t -> if List.for_all ((=) h) t then Some h else None
        in
(*
        print_endline (Show.show<expr_q list>(l));
        print_endline (Show.show<Sql.Type.t option>(t));
*)
        let assign = function
        | `Param (n,None) -> `Param (n,t)
        | x -> x
        in
        let ret = if Option.is_some ret then ret else t in
        `Func (ret,(List.map assign l)),ret
    | `Param (_,t) -> e, t
  in
  typeof expr

let show_e e = Show.show<expr_q> (e) >> print_endline

let resolve_types tables expr =
  expr
  >> resolve_columns tables
(*   >> tee show_e  *)
  >> assign_types

let infer_scheme columns tables joined_scheme =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let resolve1 = function
    | All -> joined_scheme
    | AllOf t -> scheme t
    | Expr (e,name) ->
      let col = begin
      match e with
      | `Column (name,Some t) -> RA.Scheme.find (scheme t) name
      | `Column (name,None) -> RA.Scheme.find all name (* FIXME `all` or `joined_scheme` ? *)
      | _ -> RA.attr "" (Option.default Sql.Type.Text (resolve_types tables e >> snd))
      end in
      let col = Option.map_default (fun n -> {col with RA.name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

let get_params e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Func (_,l) -> List.fold_left loop acc l
    | `Value _ -> acc
  in
  loop [] e >> List.rev

let get_params tables e =
  e >> resolve_types tables >> fst >> get_params

(*
let _ =
  let e = Sub [Value Sql.Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Sql.Type.Int);] in
  e >> get_params >> to_string >> print_endline
*)

let params_of_column tables = function
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params tables e

let params_of_columns tables = collect (params_of_column tables)

let get_params_opt tables = function
  | Some x -> get_params tables x
  | None -> []

let get_params_l tables l = collect (get_params tables) l

(* FIXME order of columns *)
let do_join (tables,params) ((table1,params1),kind) =
(*     let (name1,scheme1) = table1 in *)
  let tables = tables @ [table1] in
  let p = match kind with
  | `Cross -> []
  | `Default -> []
  | `Natural -> []
  | `Using l -> []
  | `Search e -> get_params tables e
  in
  tables,params @ params1 @ p

let join ((t0,p0),joins) =
  let (tables,params) = List.fold_left do_join ([t0],p0) joins in
  let joined_scheme = tables >> List.map snd >> List.flatten in
  (tables,params,joined_scheme)

