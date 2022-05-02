(* zadanie: Drzewa lewicowe *)
(* autor: Kacper Szczepanski *)
(* recenzent: Pawel Kowalski*)

(* typ kopca binarnego *)
(* Node (left, value, right, rh) oznacza, ze to poddrzewo ma lewego syna left, *)
(* priorytet w wierzcholku value, prawego syna right i najbardziej prawa sciezke dlugosci rh *)
type 'a queue = | Node of 'a queue * 'a * 'a queue * int
                | Leaf

(* funkcja sprawdzajaca, czy dana kolejka jest pusta *)
let is_empty q =
  match q with
  | Leaf -> true
  | Node (_, _, _, _) -> false

(* wyjatek podnoszony, gdy kolejka przekazana do delete_min jest pusta *)
exception Empty

(* funkcja zwracajaca pusta kolejke *)
let empty = Leaf

(* funkcja laczaca dwie kolejki w jedna *)
let rec join q1 q2 = 
  if is_empty q1 = true then
    q2
  else if is_empty q2 = true then
    q1
  else
    match (q1, q2) with
    | (Node (left1, value1, right1, rh1), Node (left2, value2, right2, rh2)) ->
      if value2 < value1 then
        join q2 q1
      else
        let res = join q2 right1 in
        (match (res, left1) with
        | (Node (left3, value3, right3, rh3), Leaf) -> Node (res, value1, Leaf, 1)
        | (Node (left3, value3, right3, rh3), Node (left4, value4, right4, rh4)) ->
          if rh3 <= rh4 then
            Node (left1, value1, res, rh3 + 1)
          else
            Node (res, value1, left1, rh4 + 1)
        | _ -> assert (false))
    | _ -> assert (false)

(* funkcja zwracajaca minimum na kolejce i kolejke bez najmniejszego elementu *)
let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node (left, value, right, rh) -> (value, join left right)

(* funkcja dodajaca element do kolejki *)
let add e q = join (Node (Leaf, e, Leaf, 1)) q

(* 
let a = empty;;
let a = add 0 a;;
let a = add 3 a;;
let a = add 5 a;;
let a = add 2 a;;
let a = add 2 a;;
let a = add 8 a;;

let b = empty;;
let c = join a b;;
let er = delete_min b;;
let er = is_empty b;;
let b = add 10 b;;
let c = join a b;;
let b = add 12 b;;
let c = join a b;;
let b = add 7 b;;
let c = join a b;;
*)