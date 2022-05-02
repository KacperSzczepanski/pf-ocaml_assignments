(* zadanie: Arytmetyka *)
(* autor - Kacper Szczepanski *)

(* typ "wartosc" trzyma jeden lub dwa przedzialy liczb rzeczywistych
   zmienna a * b * c dla a <= b oraz c = true odpowiada przedzialowi [a, b]
   zmienna a * b * c dla a <= b oraz c = false odpowiada sumie przedzialow (-inf, a] i [b, +inf) *)
type wartosc = Wartosc of float * float * bool

(* funkcja sprawdzajaca, czy x jest liczba *)
let is_nan x = compare x nan = 0

(* funkcja zwracajaca iloczyn a * b z kilkoma przypadkami szczegolnymi *)
let pomnoz (a, b) = 
  if (is_nan a || is_nan b) then
    nan
  else if (a = 0. || b = 0.) then
    0.
  else
    a *. b

(* funkcja zamieniajaca wartosci koncow przedzialu, jezeli zostaly zle podane (tj. a > b) *)
let popraw_konce w =
  match w with
  | Wartosc (pocz, kon, rodzaj) ->
    if (pocz <= kon) then
      Wartosc (pocz, kon, rodzaj)
    else
      Wartosc (kon, pocz, rodzaj)

(* funkcja tworzaca przedzial liczb rzeczywistych [x - p%, x + p%] *)
let wartosc_dokladnosc x p = popraw_konce (Wartosc (x -. p /. 100. *. x, x +. p /. 100. *. x, true))

(* funkcja tworzaca przedzial liczb rzeczywistych [x, y] *)
let wartosc_od_do x y = Wartosc(x, y, true)

(* funkcja tworzaca jedno-elementowy przedzial liczb rzeczywistych [x, x] *)
let wartosc_dokladna x = Wartosc(x, x, true)

(* funkcja odpowiadajaca na pytanie "Czy x nalezy do zbioru w?" *)
let in_wartosc w x =
  match w with
  | Wartosc (pocz, kon, true) -> (pocz <= x && x <= kon)
  | Wartosc (pocz, kon, false) -> not (pocz < x && x < kon)

(* funkcja zwracajaca najmniejsza wartosc ze zbioru w *)
let min_wartosc w =
  match w with
  | Wartosc (pocz, _, true) -> pocz
  | Wartosc (_, _, false) -> neg_infinity

(* funkcja zwracajaca najwieksza wartosc ze zbioru w *)
let max_wartosc w =
  match w with
  | Wartosc (_, kon, true) -> kon
  | Wartosc (_, _, false) -> infinity

(* funkcja zwracajaca srednia arytmetyczna najmniejszej i najwiekszej wartosci zbioru w *)
let sr_wartosc w = 
  match w with
  | Wartosc (pocz, kon, true) -> ((pocz +. kon) /. 2.)
  | Wartosc (_, _, false) -> nan

(* funkcja laczaca 2 zbiory w jeden zbior *)
let rec suma_przedzialow x y =
  match (x, y) with
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, true)) ->
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    (* przedzialy maja to samo min_wartosc rozne od nan i -inf *)
    else if (xpocz = ypocz) then
      Wartosc (xpocz,  max xkon ykon, true)
    (* sprawdzam, czy przedzialy maja niepuste przeciecie przy zalozeniu, *)  
    (* ze przedzial x zaczyna sie bardziej na lewo na osi liczbowej*)
    else if (xpocz < ypocz) then                
      if (xkon < ypocz) then      
        (* nie wszystkie przypadki moga zostac wywolane, *)
        (* brak przeciecia dwoch przedzialow oznacza, ze siegaja odpowiednich nieskonczonosci *)              
        Wartosc (xkon, ypocz, false)
      else
        Wartosc (xpocz, ykon, true)
    else suma_przedzialow y x
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, true)) ->
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    (* sprawdzam jak zbior jedno-przedzialowy nachodzi *)
    (* na przedzialy zbioru dwu-przedzialowego *)
    else if (ypocz <= xpocz && xkon <= ypocz) then 
      Wartosc (neg_infinity, infinity, true)       
    else if (ypocz <= xpocz) then
      suma_przedzialow (Wartosc (neg_infinity, max xpocz ykon, true)) (Wartosc (xkon, infinity, true))
    else
      suma_przedzialow (Wartosc (neg_infinity, xpocz, true)) (Wartosc (min xkon ypocz, infinity, true))
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, false)) -> suma_przedzialow y x
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, false)) ->
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    (* przedzialy zbiorow dwu-przedzialowych rozszerzaja sie *)
    (* od nieskonczonosci w strone zera; sprawdzam, czy po tym *)
    (* nie polacza sie w jeden przedzial wszystkich liczb rzeczywistych *)
    else 
      let lewo = max xpocz ypocz in             
      let prawo = min xkon ykon in              
      if (prawo <= lewo) then                   
        Wartosc (neg_infinity, infinity, true)
      else
        Wartosc (lewo, prawo, false)

(* funkcja wyznaczajaca zbior wszystkich sum wybierajac jeden skladnik ze zbioru x i drugi ze zbioru y *)
let rec plus x y =
  match (x, y) with
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, true)) -> Wartosc (xpocz +. ypocz, xkon +. ykon, true)
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, true)) -> 
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    else
      let dolna_granica = xpocz +. ykon in
      let gorna_granica = xkon +. ypocz in
      if (gorna_granica <= dolna_granica) then
        Wartosc (neg_infinity, infinity, true)
      else
        Wartosc (dolna_granica, gorna_granica, false)
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, false)) -> plus y x
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, false)) ->
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    else  
      Wartosc (neg_infinity, infinity, true)

(* funkcja zwracajaca zbior wszystkich roznic wybierajac odjemna ze zbioru x i odjemnik ze zbioru y *)
let minus x y =
  match y with
  (* doslownie x + (-y) *)
  | Wartosc (pocz, kon, rodzaj) -> plus x (Wartosc (pomnoz ((-1.), kon), pomnoz ((-1.), pocz), rodzaj))

(* fukcja zwracajaca zbior wszystkich iloczynow wybierajac jeden czynnik ze zbioru x i drugi ze zbioru y *)
let rec razy x y =
  match (x, y) with
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, true)) -> 
    let a = pomnoz (xpocz, ypocz) in
    let b = pomnoz (xpocz, ykon) in
    let c = pomnoz (xkon, ypocz) in
    let d = pomnoz (xkon, ykon) in
    Wartosc (min (min a b) (min c d), max (max a b) (max c d), true)
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, true)) ->
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    else
      let seg1 = razy (Wartosc (neg_infinity, xpocz, true)) y in
      let seg2 = razy (Wartosc (xkon, infinity, true)) y in
      suma_przedzialow seg1 seg2
  | (Wartosc (xpocz, xkon, true), Wartosc (ypocz, ykon, false)) -> razy y x
  | (Wartosc (xpocz, xkon, false), Wartosc (ypocz, ykon, false)) -> 
    if (is_nan xpocz || is_nan xkon || is_nan ypocz || is_nan ykon) then
      Wartosc (nan, nan, true)
    else
      let pom1 = razy (Wartosc (neg_infinity, xpocz, true)) (Wartosc (neg_infinity, ypocz, true)) in
      let pom2 = razy (Wartosc (neg_infinity, xpocz, true)) (Wartosc (ykon, infinity, true)) in
      let pom3 = razy (Wartosc (xkon, infinity, true)) (Wartosc (neg_infinity, ypocz, true)) in
      let pom4 = razy (Wartosc (xkon, infinity, true)) (Wartosc (ykon, infinity, true)) in
      let seg1 = suma_przedzialow pom1 pom2 in
      let seg2 = suma_przedzialow pom3 pom4 in
      suma_przedzialow seg1 seg2

(* funkcja zwracajaca zbior odwrotnosci wszystkich liczb ze zbioru x (doslownie 1 / x) *)
let odwrotnosc x =
  match x with
  | Wartosc (pocz, kon, true) ->
    if (is_nan pocz || is_nan kon) then
      Wartosc (nan, nan, true)
    else if (pocz = 0. && kon = 0.) then
      Wartosc (nan, nan, true)
    (* przedzial nie zawiera zera - proste odbicie *)
    else if (0. < pocz || kon < 0.) then          
      Wartosc (1. /. kon, 1. /. pocz, true)
    (* przedzial zawiera jednostronnie otoczone zero *)
    else if (pocz = 0.) then
      Wartosc (1. /. kon, infinity, true)         
    else if (kon = 0.) then
      Wartosc (neg_infinity, 1. /. pocz, true)
    (* przedzial zawiera obustronnie otoczone zero *)
    else
      Wartosc (1. /. pocz, 1. /. kon, false)      
  | Wartosc (pocz, kon, false) ->
    if (is_nan pocz || is_nan kon) then
      Wartosc (nan, nan, true)
    (* przedzialy nie zawieraja zera *)
    else if (pocz < 0. && 0. < kon) then          
      Wartosc (1. /. pocz, 1. /. kon, true)
    (* ktorys przedzial zawiera obustronnie otoczone zero *)
    else if (kon < 0. || 0. < pocz) then          
      Wartosc (1. /. kon, 1. /. pocz, false)      
    (* ktorys przedzial zawiera jednostronnie otoczone zero *)
    else if (kon = 0.) then                       
      Wartosc (1. /. pocz, infinity, true)        
    else
      Wartosc (neg_infinity, 1. /. kon, true)

(* funkcja zwracajaca zbior wszystkich ilorazow wybierajac dzielna ze zbioru x i dzielnik ze zbioru y *)
let podzielic x y = razy x (odwrotnosc y)

(* recenzent - Juliusz Korab-Karpowicz *)

(*

let a = wartosc_dokladnosc 100. 21.;;
let b = wartosc_dokladnosc (-100.) 44.;;

let c = wartosc_od_do 0. 0.;;
let d = wartosc_od_do neg_infinity infinity;;

let e = wartosc_od_do nan nan;;

is_nan 2007.;;
is_nan nan;;

in_wartosc a 0.;;
in_wartosc d 0.;;

sr_wartosc e;;
sr_wartosc d;;
sr_wartosc b;;

suma_przedzialow a b;;

plus a b;;

minus a b;;

razy a b;;

podzielic a c;;
podzielic a d;;
podzielic a b;;

*)