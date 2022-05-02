(* zadanie: Origami *)
(* autor: Kacper Szczepanski *)
(* recenzent : Lukasz Orlikowski *)

(* typ punktu na plaszczyznie *)
type point = float * float;;

(* typ kartki: prostokata lub kola zgietego x razy *)
type kartka = point -> int;;

let eps = 0.0000000001;;

(* funkcja porownujaca: 0 - <, 1 - =, 2 - > *)
let comp a b =
  if a < b -. eps then 0
  else if a <= b +. eps then 1
  else 2;;

let (--) (a, b) (c, d) = (a -. c, b -. d);;     (* roznica wektorow *)
let (++) (a, b) (c, d) = (a +. c, b +. d);;     (* suma wektorow *)
let (-*) (a, b) (c, d) = (b *. c -. a *. d);;   (* iloczyn wektorowy *)
let (+*) (a, b) (c, d) = (a *. c +. b *. d);;   (* iloczyn skalarny *)
let ($*) c (a, b) = (a *. c, b *. c);;          (* iloczyn wektora i liczby *)
let (//) (a, b) c = (a /. c, b /. c);;          (* iloraz wektora i liczby *)

let przebija = function
  | true -> 1
  | false -> 0;;

(* funkcja zwracajaca kwadrat modulu wektora *)  
let pot2modul (x, y) = x *. x +. y *. y;;

(* funkcja sprawdzajaca, czy punkt p = (xp, yp) nalezy do prostokata *)
(* o przeciwleglych wierzcholkach (x1, y1) i (x2, y2), gdzie x1 <= x2 oraz y1 <= y2 *)
(* i bokach rownoleglych do osi ukladu wspolrzednych *)
let prostokat (x1, y1) (x2, y2) (xp, yp) =
  przebija (comp x1 xp <> 2 && comp xp x2 <> 2 && comp y1 yp <> 2 && comp yp y2 <> 2);;

(* funkcja sprawdzajaca, czy punkt p nalezy do kola *)
(* o srodku sr i promieniu dlugosci r *)
let kolko sr r p =
  przebija (comp (sqrt (pot2modul (p -- sr))) r <> 2);;

(* odbicie punktu p wzgledem prostej przechodzacej przez srodek ukladu i punkt q *)
let odbicie p q =
  let r = p +* q in
  let s = (r $* q) // (pot2modul q) in
  let t = s -- p in
  p ++ t ++ t;;

(* funkcja sprawdzajaca ile razy punkt p przebije kartke po zgieciu *)
(* wzdluz prostej przechodzacej przez p1 i p2 *)
let zloz p1 p2 kartka p = 
  let iw = (p2 -- p1) -* (p -- p1) in
  if comp iw 0. = 1 then
    kartka p
  else if comp iw 0. = 0 then
    let odb = (odbicie (p -- p1) (p2 -- p1)) ++ p1 in
    kartka p + kartka odb
  else
    0;;

let zloz2 kartka (x, y) = zloz x y kartka;;

(* funkcja skladajaca kartke wzdluz kolejnych prostych *)
(* wyznaczanych przez kolejne pary punktow z listy *)
let skladaj lista kartka =
  List.fold_left zloz2 kartka lista;;

(*
prostokat (0., 0.) (10., 10.) (5., 5.)
prostokat (0., 0.) (10., 10.) (21., 33.)
kolko (0., 0.) 5. (3., 4.)
kolko (0., 0.) 5. (4., 4.)
let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
	  ((5., 0.), (10., 5.));
	  ((10., 0.), (0., 10.));
	  ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
	  ((6., 0.), (10., 4.));
	  ((4., 0.), (10., 6.));
	  ((2., 0.), (10., 8.));
	  ((0., 0.), (10., 10.));
	  ((0., 2.), (8., 10.));
	  ((0., 4.), (6., 10.));
	  ((0., 6.), (4., 10.));
	  ((0., 8.), (2., 10.))];;

let p2 = skladaj l1 p1
let k2 = skladaj l1 k1;;
*)