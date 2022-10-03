open S_exp
open Util

type directions = Left | Right | Down | Up
type path = directions list

let rec get_coordinates_helper (p: path): int * int = 
  match p with
  | [] -> (0, 0)
  | Left::ps -> begin match get_coordinates_helper ps with 
      | (x, 0) -> (x - 1, 0) 
      | (x, _) -> (x, 0)
    end
  | Right::ps -> let (x, y) = get_coordinates_helper ps in (x + 1, y)
  | Down::ps -> let (x, y) = get_coordinates_helper ps in (x, y + 1)
  | Up::ps -> let (x, y) = get_coordinates_helper ps in (x, y - 1)

let get_coordinates (p: path): int * int = 
  get_coordinates_helper (List.rev p)

let bfs(exp: s_exp) (num: int): s_exp option =
  let q : s_exp Queue.t = Queue.create () in
  let n : int ref = ref num in
  Queue.push exp q;
  while !n > 0 && not (Queue.is_empty q) do
    n := !n - 1;
    match Queue.pop q with
    | Lst xs -> List.iter (fun x -> Queue.push x q) xs
    | _ -> ()
  done;
  Queue.take_opt q

let read_expr e = 
  match e with
  | Lst (Sym f::args) -> Printf.sprintf "%s expressions, %d inputs\n" f (List.length args) 
  | Sym f -> f
  | Num n -> string_of_int n
  | _ -> "Can't read expression at that position\n"

let read_position (defns: defn list) (p: path) : unit =
  let (n, d) = get_coordinates p in
  match List.nth_opt defns n with
  | Some defn when d = 0 -> Printf.printf "%s: a function definition with %d arguments: %s\n" defn.name (List.length defn.args) (String.concat " and " defn.args)
  | Some defn when d = 1 -> Printf.printf "%s\n" defn.name
  | Some defn when d = 2 -> Printf.printf "%d arguments: %s\n" (List.length defn.args) (String.concat " and " defn.args)
  | Some defn when d > 2 && d - 3 < List.length defn.args -> Printf.printf "%s\n" (List.nth defn.args (d - 3))
  | Some defn when d - 3 >= List.length defn.args -> 
    begin match (bfs defn.body (d-3- (List.length defn.args))) with
      | Some e -> Printf.printf "%s\n" (read_expr e)
      | _ -> Printf.printf "no expression at that position\n"
    end
  | _ -> Printf.printf "no expression at that position\n"

let navigate (program : string) (p: path) : unit =
  let defns, _ = parse_many program |> defns_and_body in
  read_position defns p