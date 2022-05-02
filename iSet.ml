(* zadanie: Modyfikacja drzew *)
(* autor: Kacper Szczepanski *)
(* recenzent: Pawel Pawlowski *)

(* typ BST na przedzialach z wywazaniem*)
(* (left, first, second, right, height, size) oznacza: left - lewy syn, right - prawy syn *)
(* [first, second] - przedzial liczb w korzeniu, height - dlugosc sciezki z korzenia do najdalszego liscia *)
(* size - liczba elementow (nie wierzcholkow) w poddrzewie *)
type t =
  | Empty
  | Node of t * int * int * t * int * int;;

let fir (a, _) = a;;

let sec (_, b) = b;;

(* height zwraca wysokosc drzewa *)
let height = function
  | Node (_, _, _, _, h, _) -> h
  | Empty -> 0;;

(* neg zwraca liczbe przeciwna do x w postaci sumy dwoch liczb *)
(* - jest to potrzebne dla x = min_int, poniewaz chcemy uzyskac wtedy liczbe o 1 wieksza od max_int, *)
(* ale kompilator uzna max_int + 1 za min_int *)
let neg x =
  if x == min_int then (max_int, 1)
  else ((-1) * x, 0);;

(* sum zwraca sume liczb a oraz b, z uwzglednieniem wyjscia poza zakres int *)
let sum a b =
  if a == 0 then b
  else if b == 0 then a
  else if a > 0 && b < 0 then a + b
  else if a < 0 && b > 0 then a + b
  else if a > 0 && b > 0 then
    if a + b < 0 then max_int
    else a + b
  else
    if a + b > 0 then min_int
    else a + b;;

(* size zwraca liczbe elementow w poddrzewie *)
let size = function
  | Node (_, _, _, _, _, siz) -> siz
  | Empty -> 0;;

let empty = Empty;;

(* is_empty sprawdza, czy drzewo jest puste *)
let is_empty = function
  | Node (_, _, _, _, _, _) -> false
  | Empty -> true;;

(* elements zwraca liste przedzialow drzewa w kolejnosci in-order *)
let elements iSet =
  let rec inorder iset acc =
    match iset with
    | Empty -> acc
    | Node (left, first, second, right, _, _) ->
      let res = inorder right acc in
      inorder left ((first, second) :: res)
  in inorder iSet [];;

(* iter zwraca wartosci podanej funkcji dla przedzialow drzewa w kolejnosci in-order *)
let rec iter f iSet =
  match iSet with
  | Node (left, first, last, right, _, _) ->
    iter f left; f (first, last); iter f right
  | Empty -> ();;

(* dla listy przedzialow [x1, x2, ..., xn] z drzewa podanych w kolejnosc in-order *)
(* i podanego a fold zwraca zlozenie podanej funkcji w postaci f xn (... (f x2 (f x1 a)) ... ) *)
let fold f iSet a =
  let list = elements iSet in
  let rec loop list acc =
    match list with
    | [] -> acc
    | h :: t -> loop t (f h acc)
  in loop list a;;

(* mem sprawdza, czy x nalezy do jakiegokolwiek przedzialu drzewa *)
let rec mem x iSet =
  match iSet with
  | Node (left, first, last, right, _, _) ->
    if (first <= x && x <= last) then true
    else if (x < first) then mem x left
    else mem x right
  | Empty -> false;;

(* below zwraca liczbe elementow w drzewie mniejszych lub rownych n *)
let below n iSet =
  let rec inorder iset acc =
    match iset with
    | Node (left, first, last, right, _, siz) ->
      if (first <= n && n <= last) then
        inorder left ( (* acc + n - first + 1 *)
        let odwf = neg first in
        let f = fir odwf in
        let s = sec odwf in
        if 0 <= first then
          sum (sum (sum f acc) n) 1
        else if 0 <= n then
          sum (sum (sum f acc) n) (1 + s)
        else
          sum (sum (sum n acc) f) (1 + s))
      else if n < first then
        inorder left acc
      else
        inorder right ( (* (size left) + siz + last - first + 1 *)
          let odwf = neg first in
          let f = fir odwf in
          let s = sec odwf in
        if 0 <= first then
          sum (sum (sum (sum f acc) (size left)) last) 1
        else if 0 <= last then
          sum (sum (sum (sum f acc) (size left)) last) (1 + s)
        else
          sum (sum (sum (sum last acc) (size left)) f) (1 + s))
      | Empty -> acc 
  in inorder iSet 0;;

(* make tworzy drzewo, gdzie left jest lewym synem, [first, last] *)
(* jest przedzialem w korzeniu, a right jest prawym synem *)
let make left first last right =
  Node (left, first, last, right, max (height left) (height right) + 1, (
    (* (size left) + (size right) + last - first + 1 *)
    let odwf = neg first in
    let f = fir odwf in
    let s = sec odwf in
    if 0 <= first then
      sum (sum (sum (sum f (size right)) (size left)) last) 1
    else if 0 <= last then
      sum (sum (sum (sum f (size right)) (size left)) last) (1 + s)
    else
      sum (sum (sum (sum last (size right)) (size left)) f) (1 + s)));;

(* balance dokonuje jednokrotnego balansu poddrzewa o synach left i right *)
(* oraz przedziale w korzeniu (first, last) *)
let balance left first last right =
  let hl = height left in
  let hr = height right in
  if hl > hr + 2 then
    match left with
    | Node (left2, first2, last2, right2, _, _) ->
      if height left2 >= height right2 then
        make left2 first2 last2 (make right2 first last right)
      else 
        (match right2 with
        | Node (left3, first3, last3, right3, _, _) ->
          make (make left2 first2 last2 left3) first3 last3 (make right3 first last right)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match right with
    | Node (left2, first2, last2, right2, _, _) ->
      if height right2 >= height left2 then
        make (make left first last left2) first2 last2 right2
      else
        (match left2 with
        | Node (left3, first3, last3, right3, _, _) ->
          make (make left first last left3) first3 last3 (make right3 first2 last2 right2)
        | Empty -> assert false)
    | Empty -> assert false
 else make left first last right;;

(* add_one dodaje do drzewa jeden przedzial [x, y] zakladajac, *)
(* ze bedzie rozlaczny z kazdym innym przedzialem *)
let rec add_one (x, y) iSet =
  match iSet with
  | Node (left, first, last, right, _, _) ->
    if x < first then
      balance (add_one (x, y) left) first last right
    else
      balance left first last (add_one (x, y) right)
  | Empty -> make Empty x y Empty;;

(* mini zwraca przedzial o minimalnych elementach w drzewie *)
let rec mini = function
  | Node (Empty, first, last, _, _, _) -> (first, last)
  | Node (left, _, _, _, _, _) -> mini left
  | Empty -> assert false;;

(* maxi zwraca przedzial o maksymalnych elementach w drzewie *)
let rec maxi = function
  | Node (_, first, last, Empty, _, _) -> (first, last)
  | Node (_, _, _, right, _, _) -> maxi right
  | Empty -> assert false;;

(* remove_mini zwraca drzewo bez minimalnego przedzialu *)
let rec remove_mini = function
  | Node (Empty, _, _, right, _, _) -> right
  | Node (left, first, last, right, _, _) -> balance (remove_mini left) first last right
  | Empty -> assert false;;

(* remove_maxi zwraca drzewo bez maksymalnego przedzialu *)
let rec remove_maxi = function
  | Node (left, _, _, Empty, _, _) -> left
  | Node (left, first, last, right, _, _) -> balance left first last (remove_maxi right)
  | Empty -> assert false;;

(* join balansuje cale drzewo o synach left i right oraz korzeniu [first, last] *)
let rec join left first last right =
  match left, right with
  | Empty, _ -> add_one (first, last) right
  | _, Empty -> add_one (first, last) left
  | Node (left1, first1, last1, right1, height1, _), Node (left2, first2, last2, right2, height2, _) ->
    if height1 > height2 + 2 then 
      balance left1 first1 last1 (join right1 first last right)
    else if height2 > height1 + 2 then
      balance (join left first last left2) first2 last2 right2
    else
      make left first last right;;

(* join2 balansuje drzewo zlozone z polaczenia drzew left i right *)
let join2 left right =
  match left, right with
  | Empty, _ -> right
  | _, Empty -> left
  | _ ->
    if height left > height right then
      let root = maxi left in
      join (remove_maxi left) (fir root) (sec root) right
    else
      let root = mini right in
      join left (fir root) (sec root) (remove_mini right);;

(* merge laczy 2 drzewa i dokonuje jednokrotnego balansu *)
let merge iSet1 iSet2 =
  match iSet1, iSet2 with
  | Empty, _ -> iSet2
  | _, Empty -> iSet1
  | _, _ -> 
    let root = mini iSet2 in
    balance iSet1 (fir root) (sec root) (remove_mini iSet2);;

(* less2 odpowiada, czy roznica miedzy x a y jest przynajmniej 2 *)
let less2 x y =
  if x == y then false
  else if x == min_int && x + 1 < y then true
  else if x < y - 1 && y == max_int then true
  else if max_int - 1 <= x then false
  else if y <= min_int + 1 then false
  else (x + 1 < y);;

(* split dzieli drzewo przez wierzcholek x - wynikiem jest *)
(* (drzewo o elementach mniejszych od x, czy x znajduje sie w drzewie, drzewo o elementach wiekszych od x) *)
let rec split x iSet =
  match iSet with
  | Empty -> (Empty, false, Empty)
  | Node (left, first, last, right, _, _) ->
    if first <= x && x <= last then
      if first == last then
        (left, true, right)
      else if first == x then
        (left, true, add_one (x + 1, last) right)
      else if x == last then
        (add_one (first, x - 1) left, true, right)
      else
        (add_one (first, x - 1) left, true, add_one (x + 1, last) right)
    else if x < first then
      let (lres, bres, rres) = split x left in (lres, bres, join rres first last right)
    else
      let (lres, bres, rres) = split x right in (join left first last lres, bres, rres);;
  

(* remove usuwa z drzewa wszystkie elementy z zakresu [x, y] *)
let rec remove (x, y) iSet =
  let (left, _, iset) = split x iSet in
  let (_, _, right) = split y iset in
  join2 left right;;

(* split2 dziala podobnie do split, ale usuwa caly przedzial ktory zawiera x, *)
(* do tego zwraca jego konce *)
let rec split2 x iSet =
  match iSet with
  | Empty -> (Empty, Empty, max_int, min_int)
  | Node (left, first, last, right, _, _) ->
    if first <= x && x <= last then
      (left, right, first, last)
    else if x < first then
      let (lres, rres, min_val, max_val) = split2 x left in (lres, join rres first last right, min_val, max_val)
    else
      let (lres, rres, min_val, max_val) = split2 x right in (join left first last lres, rres, min_val, max_val);;
   
(* add dodaje liczby [x, y] do drzewa *)
let add (x, y) iSet =
  let (left, iset, min_val1, max_val1) = split2 (min x (x - 1)) iSet in
  let (_, right, min_val2, max_val2) = split2 (max y (y + 1)) iset in
  let res = join2 left right in
  add_one (min (min min_val1 min_val2) x, max (max max_val1 max_val2) y) res;;

(* 
(Node (Node (Node (Empty, 1, 1, Empty, 0, 1), 3, 3, Node (Empty, 4, 4, Empty, 0, 1), 0, 3), 5, 5, Node (Node (Empty, 6, 6, Empty, 0, 1), 7, 7, Empty, 0, 2), 0, 6))
(Node (Node (Node (Empty, 1, 1, Empty, 0, 1), 3, 3, Node (Empty, 4, 4, Empty, 0, 1), 0, 3), 5, 5, Node (Empty, 6, 6, Empty, 0, 1), 0, 5))
(Node (Empty, 1, 1, Node (Empty, 2, 2, Node (Empty, 3, 3, Empty, 0, 1), 0, 2), 0, 3))
(Node (Empty, 1, 1, Node (Node (Empty, 2, 2, Empty, 0, 1), 3, 3, Empty, 0, 2), 0, 3))
let s = (Node (Node (Node (Empty, 1, 1, Empty, 0, 1), 3, 3, Node (Empty, 4, 4, Empty, 0, 1), 0, 3), 5, 5, Node (Node (Empty, 6, 6, Empty, 0, 1), 7, 7, Empty, 0, 2), 0, 6));;
let s = empty;;
let s = add (1, 1) s;;
let s = add (3, 3) s;;
let s = add (5, 5) s;; 
let s = add (7, 7) s;;
let s = add (9, 9) s;;
let s = add (11, 11) s;;
let s = add (13, 13) s;;
let s = add (15, 15) s;;
*)