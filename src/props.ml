open ExtLib
(** Association list *)

type t = (string * string) list

let get x n = try Some (List.assoc n x) with Not_found -> None

let get_all x n =
  List.filter_map (fun (k, v) -> if k = n then Some v else None) x

let set x n v = (n, v) :: x

let empty = []
