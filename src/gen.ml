(* Code generation *)

open Printf
open ExtLib
open Prelude
open Stmt

type subst_mode = Named | Unnamed | Oracle | PostgreSQL

type stmt = {
  schema : Sql.Schema.t;
  vars : Sql.var list;
  kind : kind;
  props : Props.t;
}

(** defines substitution function for parameter literals *)
let params_mode = ref None

let inc_indent, dec_indent, make_indent =
  let v = ref 0 in
  ( (fun () -> v := !v + 2),
    (fun () -> v := !v - 2),
    fun () -> String.make !v ' ' )

let print_indent () = print_string (make_indent ())

let indent s =
  print_indent ();
  print_string s

let indent_endline s =
  print_indent ();
  print_endline s

let output fmt = kprintf indent_endline fmt

let output_l = List.iter indent_endline

let print fmt = kprintf print_endline fmt

let indented k =
  inc_indent ();
  k ();
  dec_indent ()

let name_of attr index =
  match attr.Sql.name with "" -> sprintf "_%u" index | s -> s

let make_param_name index ((name, _) : Sql.param_id) =
  match name with None -> sprintf "_%u" index | Some s -> s

let show_param_name (p : Sql.param) index = make_param_name index (fst p)

let param_type = snd

let make_name props default = Option.default default (Props.get props "name")

let default_name str index = sprintf "%s_%u" str index

let choose_name props kind index =
  let fix =
    String.map (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
      | _ -> '_')
  in
  let fix s =
    match Props.get props "subst" with
    | Some x ->
        let _, s = String.replace ~str:s ~sub:("%%" ^ x ^ "%%") ~by:x in
        fix s
    | None -> fix s
  in
  let name =
    match kind with
    | Create t -> sprintf "create_%s" (fix t)
    | CreateIndex t -> sprintf "create_index_%s" (fix t)
    | Update (Some t) -> sprintf "update_%s_%u" (fix t) index
    | Update None -> sprintf "update_%u" index
    | Insert (_, t) -> sprintf "insert_%s_%u" (fix t) index
    | Delete t -> sprintf "delete_%s_%u" (fix t) index
    | Alter t -> sprintf "alter_%s_%u" (fix t) index
    | Drop t -> sprintf "drop_%s" (fix t)
    | Select _ -> sprintf "select_%u" index
    | Other -> sprintf "statement_%u" index
  in
  make_name props name

type sql =
  | Static of string
  | Dynamic of
      (Sql.param_id * (Sql.param_id * Sql.var list option * sql list) list)

let substitute_vars s vars subst_param =
  let rec loop acc i parami vars =
    match vars with
    | [] -> (acc, i)
    | Sql.Single (((_, (i1, i2)), _) as param) :: tl ->
        let acc, parami =
          match subst_param with
          | None -> (Static (String.slice ~first:i ~last:i2 s) :: acc, parami)
          | Some subst ->
              ( Static (subst parami param)
                :: Static (String.slice ~first:i ~last:i1 s)
                :: acc,
                parami + 1 )
        in
        loop acc i2 parami tl
    | Choice (((_, (i1, i2)) as name), ctors) :: tl ->
        let acc = Static (String.slice ~first:i ~last:i1 s) :: acc in
        let acc =
          Dynamic
            ( name,
              ctors
              |> List.map (function
                   | Sql.Simple (((_, (c1, c2)) as ctor), args) -> (
                       ( ctor,
                         args,
                         match args with
                         | None -> [ Static "" ]
                         | Some l ->
                             let acc, last = loop [] c1 0 l in
                             List.rev
                               ( Static (String.slice ~first:last ~last:c2 s)
                               :: acc ) ) )
                   | Verbatim (n, v) ->
                       ((Some n, (0, 0)), Some [], [ Static v ])) )
          :: acc
        in
        loop acc i2 parami tl
  in
  let acc, last = loop [] 0 0 vars in
  let acc = List.rev (Static (String.slice ~first:last s) :: acc) in
  let rec squash acc = function
    | [] -> List.rev acc
    | Static s1 :: Static s2 :: tl -> squash acc (Static (s1 ^ s2) :: tl)
    | x :: xs -> squash (x :: acc) xs
  in
  squash [] acc

let subst_named index p = "@" ^ show_param_name p index

let subst_oracle index p = ":" ^ show_param_name p index

let subst_postgresql index _ = "$" ^ string_of_int (index + 1)

let subst_unnamed _ _ = "?"

let get_sql stmt =
  let sql = Props.get stmt.props "sql" |> Option.get in
  let subst =
    match !params_mode with
    | None -> None
    | Some subst ->
        Some
          ( match subst with
          | Named -> subst_named
          | Unnamed -> subst_unnamed
          | Oracle -> subst_oracle
          | PostgreSQL -> subst_postgresql )
  in
  substitute_vars sql stmt.vars subst

let get_sql_string_only stmt =
  match get_sql stmt with
  | [ Static s ] -> s
  | _ -> fail "dynamic choices not supported for this language"

let time_string () =
  let module U = Unix in
  let t = U.time () |> U.gmtime in
  sprintf "%04u-%02u-%02uT%02u:%02uZ" (1900 + t.U.tm_year) (t.U.tm_mon + 1)
    t.U.tm_mday t.U.tm_hour t.U.tm_min

module type LangTypes = sig
  val as_api_type : Sql.Type.t -> string

  val as_lang_type : Sql.Type.t -> string
end

module Translate (T : LangTypes) = struct
  let show_param_type (_, t) = T.as_api_type t

  let schema_to_values =
    List.mapi (fun i attr -> (name_of attr i, T.as_lang_type attr.Sql.domain))

  (* let schema_to_string = G.Values.to_string $ schema_to_values  *)
  let all_params_to_values l =
    l
    |> List.mapi (fun i p ->
           (show_param_name p i, T.as_lang_type @@ param_type p))
    |> List.unique ~cmp:(fun (n1, _) (n2, _) -> String.equal n1 n2)

  (* rev unique rev -- to preserve ordering with respect to first occurrences *)
  let values_of_params =
    List.rev $ List.unique ~cmp:( = ) $ List.rev $ all_params_to_values

  let names_of_vars l =
    l
    |> List.mapi (fun i v ->
           make_param_name i
             (match v with Sql.Single (id, _) -> id | Choice (id, _) -> id))
    |> List.unique ~cmp:String.equal

  let params_only =
    List.map (function
      | Sql.Single p -> p
      | Choice _ -> fail "dynamic choices not supported for this host language")
end

module type Generator = sig
  type t

  val generate : t -> string -> stmt list -> unit

  val start : unit -> t

  val comment : t -> ('a, unit, string, unit) format4 -> 'a

  val empty_line : t -> unit
end

module Make (S : Generator) = struct
  let generate_header out mode =
    S.comment out "DO NOT EDIT MANUALLY";
    S.comment out "";
    let time_str =
      match mode with
      | `Full -> " on " ^ time_string ()
      | `Without_timestamp -> ""
    in
    S.comment out "generated by sqlgg %s%s" Sqlgg_config.version time_str;
    S.empty_line out

  let process name stmts =
    let out = S.start () in
    Option.may (generate_header out) !Sqlgg_config.gen_header;
    S.generate out name stmts
end
