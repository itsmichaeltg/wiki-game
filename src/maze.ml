open! Core

module Position = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include Comparable.Make (T)
end

module Maze = struct
  type t =
    | START
    | END
    | PATH
    | WALL
  [@@deriving sexp_of, enumerate, compare, equal]
end

let read_file input_file =
  List.map
    (In_channel.read_lines (File_path.to_string input_file))
    ~f:(fun i ->
      String.substr_replace_all ~pattern:"" ~with_:" " i
      |> String.split ~on:' '
      |> List.filter ~f:(fun i -> not (String.equal i "")))
;;

let get_map input_file =
  List.foldi
    (read_file input_file)
    ~init:Position.Map.empty
    ~f:(fun idx map i ->
      List.foldi ~init:map i ~f:(fun idx2 map j ->
        match j with
        | "S" -> Map.add_exn map ~key:(idx, idx2) ~data:Maze.START
        | "E" -> Map.add_exn map ~key:(idx, idx2) ~data:Maze.END
        | "." -> Map.add_exn map ~key:(idx, idx2) ~data:Maze.PATH
        | "#" -> Map.add_exn map ~key:(idx, idx2) ~data:Maze.WALL
        | _ -> map))
;;

let get_neighbor_coord ~index_tup =
  let x, y = index_tup in
  [ x + 1, y; x - 1, y; x, y - 1; x, y + 1 ]
;;

let rec dfs_solver ~map ~seen ~vertex ~solved =
  match !solved with
  | true -> []
  | _ ->
    (match Map.find map vertex with
     | Some Maze.PATH | Some Maze.START ->
       (match Set.mem seen vertex with
        | true -> []
        | false ->
          let seen = Set.add seen vertex in
          let children = get_neighbor_coord ~index_tup:vertex in
          List.concat_map children ~f:(fun i ->
            match dfs_solver ~map ~seen ~vertex:i ~solved with
            | _ :: _ as list ->
              solved := true;
              vertex :: list
            | [] -> []))
     | Some Maze.END -> [ vertex ]
     | _ -> [])
;;

let solve ~input_file =
  let map = get_map input_file in
  List.iter
    (dfs_solver
       ~map
       ~seen:Position.Set.empty
       ~vertex:
         (Map.filter_keys map ~f:(fun key ->
            Maze.equal (Map.find_exn map key) Maze.START)
          |> Map.keys
          |> List.hd_exn)
       ~solved:(ref false))
    ~f:(fun (x, y) -> printf "(%d, %d)\n" x y)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () -> solve ~input_file]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;

let test ~input_file = solve ~input_file

let%expect_test "test solve_maze" =
  (* test ~input_file:(File_path.of_string "../resources/maze_small.txt"); *)
  test ~input_file:(File_path.of_string "../resources/maze_medium.txt");
  (* [%expect {| (1, 0) (1, 1) (1, 2) (1, 3) (1, 4) (2, 4) (2, 5) |}]; *)
  [%expect
    {|
        (1, 0)                               
        (1, 1)
        (2, 1)
        (3, 1)
        (4, 1)
        (5, 1)
        (5, 2)
        (5, 3)
        (5, 4)
        (5, 5)
        (4, 5)
        (3, 5)
        (3, 6)
      |}]
;;
