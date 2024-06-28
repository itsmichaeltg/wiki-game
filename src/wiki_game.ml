open! Core
module Article = String

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
parse contents
  $$ "a[href*='/wiki/']"
  |> to_list |> List.filter_map ~f:(fun li -> 
    let href = R.attribute "href" li in
    match Wikipedia_namespace.namespace href with 
    | None -> Some href
    | Some _ -> None
    ) |> Set.of_list (module String) |> Set.to_list
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module G = Graph.Imperative.Graph.Concrete (Article)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = sprintf {|"%s"|} v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let set_vertex ~vertex ~how_to_fetch = 
  match how_to_fetch with
  | File_fetcher.How_to_fetch.Remote -> String.concat ["https://en.wikipedia.org"; vertex]
  | _ -> vertex
;;

let get_title ~vertex ~how_to_fetch= 
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:vertex in
  let open Soup in
  parse contents $ "title" |> R.leaf_text 
;;

let rec get_adjacency_matrix ~max_depth ~how_to_fetch ~matrix ~worklist ~next_worklist ~seen = 
  match max_depth with 
  | 0 -> matrix
  |_ ->
  match List.length worklist with 
  | 0 -> 
    let worklist = next_worklist in
    let next_worklist = [] in
    get_adjacency_matrix ~max_depth:(max_depth - 1) ~how_to_fetch ~matrix ~worklist ~seen ~next_worklist
  | _ -> 
    let vertex = set_vertex ~vertex:(List.hd_exn worklist) ~how_to_fetch in
    print_s [%message (vertex:string)];
    match Set.mem seen vertex with 
    | true -> 
      let worklist = List.tl_exn worklist in
      get_adjacency_matrix ~max_depth ~how_to_fetch ~matrix ~worklist ~seen ~next_worklist
    | _ ->
    let seen = Set.add seen vertex in
    let contents = File_fetcher.fetch_exn how_to_fetch ~resource:vertex in
    let linked_articles = get_linked_articles contents in
    let matrix = matrix @ [[get_title ~vertex ~how_to_fetch] @ List.map linked_articles ~f:(fun i -> get_title ~vertex:i ~how_to_fetch)] in
    let worklist = List.tl_exn worklist in
    let next_worklist = next_worklist @ linked_articles in
    get_adjacency_matrix ~max_depth ~how_to_fetch ~matrix ~worklist ~seen ~next_worklist
;;

let add_edge_between_aticles lst ~graph =
  match lst with 
  | head :: tail -> List.iter tail ~f:(fun i -> G.add_edge graph head i); graph
  | [] -> graph
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let matrix = [] in
  let seen = Set.empty (module String) in
  let origin = String.substr_replace_all ~pattern:"https://en.wikipedia.org" ~with_:"" origin in
  let worklist = [origin] in
  let matrix = get_adjacency_matrix ~max_depth ~how_to_fetch ~matrix ~next_worklist:[]
              ~worklist ~seen in
  let graph = List.fold matrix ~init:(G.create ()) ~f:(fun graph i -> add_edge_between_aticles i ~graph) in
  Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let rec dfs ~max_depth ~destination ~vertex ~seen ~how_to_fetch ~found =
  match !found with
  | true -> []
  | _ ->
    match String.equal vertex destination with 
    | true -> found := true; [get_title ~vertex ~how_to_fetch]
    | _ ->
      match max_depth with
      | 0 -> []
      | _ -> 
        match Set.mem seen vertex with 
          | true -> []
          | false -> 
              let seen = Set.add seen vertex in
              let vertex = set_vertex ~vertex ~how_to_fetch in
              let contents = File_fetcher.fetch_exn how_to_fetch ~resource:vertex in
              List.concat_map (get_linked_articles contents) ~f:(fun i ->
              match (dfs ~max_depth:(max_depth - 1) ~destination ~seen ~vertex:i ~how_to_fetch ~found) with
              | _ :: _ as list -> (get_title ~vertex ~how_to_fetch) :: list
              | [] -> [])
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let origin = String.substr_replace_all ~pattern:"https://en.wikipedia.org" ~with_:"" origin in
  match dfs ~max_depth ~destination ~vertex:origin ~seen:String.Set.empty ~how_to_fetch ~found:(ref false) with
  | [] -> None
  | lst -> Some lst
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;

let print_str_lst lst = List.iter lst ~f:(fun i -> printf "%s\n" i)

let test ~content = 
  let linked_articles = get_linked_articles content |> List.dedup_and_sort ~compare:String.compare in
  Stdio.printf !"Linked Articles: "; print_str_lst linked_articles;
;;

let %expect_test "test get_linked_articles" = 
  let content = File_fetcher.fetch_exn Remote ~resource:"https://en.wikipedia.org/wiki/Endara" in
  test ~content;
  [%expect {|
        Linked Articles: /wiki/Endara
        /wiki/Given_name
        /wiki/Gonzalo_Endara_Crow
        /wiki/Guido_J._Martinelli_Endara
        /wiki/Guillermo_Endara
        /wiki/Iv%C3%A1n_Endara
        /wiki/Main_Page
        /wiki/Surname 
      |}]
;;