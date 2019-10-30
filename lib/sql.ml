open! Base

module Type = struct
  type t = Int | Text | Blob | Float | Bool | Datetime | Any
  [@@deriving compare, sexp]

  let to_string x = [%sexp_of: t] x |> Base.Sexp.to_string_hum

  type tyvar = Typ of t | Var of int [@@deriving compare, sexp]

  type func =
    | Group of t (* _ -> t *)
    | Agg (* 'a -> 'a *)
    | Multi of tyvar * tyvar (* 'a -> ... -> 'a -> 'b *)
    | Ret of t (* _ -> t *)
    (* TODO eliminate *)
    | F of tyvar * tyvar list
  [@@deriving compare, sexp]

  let is_grouping = function
    | Group _ | Agg -> true
    | Ret _ | F _ | Multi _ -> false
end

module Constraint = struct
  module T = struct
    type conflict_algo = Ignore | Replace | Abort | Fail | Rollback
    [@@deriving compare, sexp]

    type t =
      | PrimaryKey
      | NotNull
      | Null
      | Unique
      | Autoincrement
      | OnConflict of conflict_algo
    [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

type attr = { name : string; domain : Type.t; extra : Set.M(Constraint).t }
[@@deriving compare, sexp]

let make_attribute name domain extra = { name; domain; extra }

module Schema = struct
  type t = attr list [@@deriving compare, sexp]

  exception Error of t * string
end

type num_op = [ `Add | `Sub | `Mul | `Div | `Mod ] [@@deriving compare, sexp]

type bool_op = [ `And | `Or | `Not ] [@@deriving compare, sexp]

type cmp_op = [ `Eq | `Neq | `Ge | `Le | `Gt | `Lt ] [@@deriving compare, sexp]

type bit_op = [ `Bit_and | `Bit_not | `Bit_or | `Lsh | `Rsh ]
[@@deriving compare, sexp]

type agg_op = [ `Count | `Avg | `Sum | `Min | `Max ] [@@deriving compare, sexp]

type binop =
  [ `Add
  | `And
  | `Div
  | `Eq
  | `Ge
  | `Gt
  | `Le
  | `Lt
  | `Mod
  | `Mul
  | `Or
  | `Sub ]
[@@deriving compare, sexp]

type unop = [ `IsNull | `Not ] [@@deriving compare, sexp]

type op =
  [ num_op
  | bool_op
  | cmp_op
  | bit_op
  | agg_op
  | `Between
  | `Call of string
  | `Concat
  | `In
  | `Interval of unit
  | `Is
  | `IsDistinct
  | `IsNull
  | `Ite
  | `Like
  | `NotDistinct
  | `Substring ]
[@@deriving compare, sexp]

type schema = Schema.t [@@deriving compare, sexp]

type param_id = string option * (int * int) [@@deriving compare, sexp]
(** optional name and start/end position in string *)

type param = param_id * Type.t [@@deriving compare, sexp]

type params = param list [@@deriving compare, sexp]

type ctor =
  | Simple of param_id * var list option
  | Verbatim of string * string

and var = Single of param | Choice of param_id * ctor list
[@@deriving compare, sexp]

type vars = var list [@@deriving compare, sexp]

type alter_pos = [ `After of string | `Default | `First ]
[@@deriving compare, sexp]

type alter_action =
  [ `Add of attr * alter_pos
  | `RenameTable of string
  | `RenameColumn of string * string
  | `RenameIndex of string * string
  | `Drop of string
  | `Change of string * attr * alter_pos
  | `None ]
[@@deriving compare, sexp]

type select_result = schema * param list [@@deriving compare, sexp]

type direction = [ `Asc | `Desc ] [@@deriving compare, sexp]

type int_or_param = [ `Const of int | `Limit of param ]
[@@deriving compare, sexp]

type limit_t = [ `Limit | `Offset ] [@@deriving compare, sexp]

type col_name = {
  cname : string;  (** column name *)
  tname : string option;  (** table name *)
}
[@@deriving compare, sexp]

type limit = param list * bool [@@deriving compare, sexp]

type 'expr choices = (param_id * 'expr option) list [@@deriving compare, sexp]

type value =
  | Int of int
  | Date of string
  | String of string
  | Bool of bool
  | Float of float
  | Null
[@@deriving compare, sexp]

type 'f nested = 'f source * ('f source * 'f join_cond) list
[@@deriving compare, sexp]

and 'f source1 =
  [ `Select of 'f select_full | `Table of string | `Nested of 'f nested ]
[@@deriving compare, sexp]

and 'f source = 'f source1 * string option [@@deriving compare, sexp]

and 'f join_cond =
  [ `Cross | `Search of 'f expr | `Default | `Natural | `Using of string list ]
[@@deriving compare, sexp]

and 'f select = {
  columns : 'f column list;
  from : 'f nested option;
  where : 'f expr option;
  group : 'f expr list;
  having : 'f expr option;
}
[@@deriving compare, sexp]

and 'f select_full = {
  select : 'f select * 'f select list;
  order : 'f order;
  limit : limit option;
}
[@@deriving compare, sexp]

and 'f order = ('f expr * direction option) list [@@deriving compare, sexp]

and 'f expr =
  | Value of value  (** literal value *)
  | Sequence of 'f expr list
  | Param of param
  | Choices of param_id * 'f expr choices
  | Fun of 'f * 'f expr list  (** parameters *)
  | Select of 'f select_full * [ `AsValue | `Exists ]
  | Column of col_name
  | Inserted of string  (** inserted value *)
[@@deriving compare, sexp]

and 'f column =
  | All
  | AllOf of string
  | Expr of 'f expr * string option  (** name *)
[@@deriving compare, sexp]

type 'f columns = 'f column list [@@deriving compare, sexp]

type 'f expr_q =
  [ `Value of Type.t  (** literal value *)
  | `Param of param
  | `Choice of param_id * 'f expr_q choices
  | `Func of Type.func * 'f expr_q list
    (** return type, grouping, parameters *) ]
[@@deriving compare, sexp]

type 'f assignments = (col_name * 'f expr) list [@@deriving compare, sexp]

type 'f insert_action = {
  target : string;
  action :
    [ `Set of 'f assignments option
    | `Values of string list option * 'f expr list list option
    | (* column names * list of value tuples *)
      `Select of
      string list option * 'f select_full ];
  on_duplicate : 'f assignments option;
}
[@@deriving compare, sexp]

type 'f stmt =
  | Create of string * [ `Schema of schema | `Select of 'f select_full ]
  | Drop of string
  | Alter of string * alter_action list
  | Rename of (string * string) list
  | CreateIndex of string * string * string list (* index name, table name, columns *)
  | Insert of 'f insert_action
  | Delete of string * 'f expr option
  | Set of string * 'f expr
  | Update of string * 'f assignments * 'f expr option * 'f order * param list (* where, order, limit *)
  | UpdateMulti of 'f source list * 'f assignments * 'f expr option
  | Select of 'f select_full
  | CreateRoutine of
      string * Type.t option * (string * Type.t * 'f expr option) list
[@@deriving compare, sexp]
