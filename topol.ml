(* zadanie: Sortowanie topologiczne *)
(* autor: Kacper Szczepanski *)
(* recenzent: Adam Cichy *)

(* wyjatek podnoszony, gdy graf wejsciowy ma cykl *)
exception Cykliczne;;

(* funkcja zwracajaca jedno z sortowan topologicznych grafu *)
let topol graph =
  let edges = ref (PMap.create compare) in   (* mapa krawedzi w postaci pary (wierzcholek, lista sasiadow) *)
  let vis = ref (PMap.create compare) in     (* mapa stanow w postaci pary (wierzcholek, stan) *)
  let res = ref [] in                        (* lista wynikowa *)

  (* dla kazdego wierzcholka w grafie tworzymy pusta liste sasiedztwa *)
  (* oraz ustawiamy stan na 0 *)
  List.iter (fun (node, list) ->
    edges := PMap.add node [] !edges;
    vis := PMap.add node 0 !vis;
    List.iter (fun node2 ->
      edges := PMap.add node2 [] !edges;
      vis := PMap.add node2 0 !vis)
    list)
  graph;
  
  (* kazdemu wierzcholkowi aktualizujemy liste sasiadow (tych, do ktorych mozna z niego dojsc) *)
  List.iter (fun (node, list) ->
    List.iter (fun node2 ->
      edges := PMap.add node (node2 :: (PMap.find node !edges)) !edges)
    list)
  graph;

  (* funkcja akutalizujaca liste posortowania topologicznego zaczynajac z podgrafu o korzeniu node *)
  (* stan 0 - wierzcholek nieodwiedzony *)
  (* stan 1 - wierzcholek odwiedzony i nieaktywny *)
  (* stan -1 - wierzcholek odwiedzony i aktywny, czyli taki, z ktorego jeszcze nie wyszlismy *)
  (* jezeli natrafimy na wierzcholek aktywny oznacza to, ze przeszlismy po cyklu *)
  let rec dfs node =
    (vis := PMap.add node (-1) !vis;
    let neighbours = PMap.find node !edges in
    List.iter (fun neigh ->
      let t = PMap.find neigh !vis in
      if t = (-1) then raise Cykliczne
      else if t = 0 then dfs neigh
    ) neighbours;
    vis := PMap.add node 1 !vis;
    res := (node :: !res)) in

  (* wywolanie funkcji dfs dla kazdego nieodwiedzonego do tej pory wierzcholka *)
  PMap.iter (fun a b -> 
    if (PMap.find a !vis) = 0 then dfs a
    else ()) !edges;

  !res;;
  
(* 
let a = [(1, [2; 3]); (7, [])];;
topol a;;

let a = [(1, [2]); (2, [3]); (3, [4]); (5, [1]); (4, [5])];;
topol a;;

let a = [(1, [])];;
topol a;;

let a = [(1, [1])];;
topol a;;

let a = [(1, [2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2])];;
topol a;;
*)