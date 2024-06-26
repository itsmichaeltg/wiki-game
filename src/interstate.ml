open! Core
module City = String

module Highways = struct
  module Connection = struct
    module T = struct
      type t = City.t list [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let of_stirng s =
      match
        String.split
          (String.substr_replace_all ~pattern:"." s ~with_:""
           |> String.substr_replace_all ~pattern:" " ~with_:"")
          ~on:','
      with
      | lst -> Some (List.tl_exn lst)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Connection.of_stirng s with
        | Some a -> [ a ]
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    Connection.Set.of_list connections |> Set.to_list |> List.tl_exn
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let highway = Highways.of_file input_file in
        printf !"%{sexp: Highways.Connection.T.t list}\n" highway]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let rec add_edge_between_cities ~head tail ~graph =
  match List.length tail with
  | 0 -> graph
  | _ ->
    List.iter tail ~f:(fun i -> G.add_edge graph head i);
    add_edge_between_cities
      ~head:(List.hd_exn tail)
      (List.tl_exn tail)
      ~graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let highway = Highways.of_file input_file in
        let graph =
          List.fold highway ~init:(G.create ()) ~f:(fun graph i ->
            add_edge_between_cities
              ~head:(List.hd_exn i)
              (List.tl_exn i)
              ~graph)
        in
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
