(* zadanie: Przelewanka *)
(* autor: Kacper Szczepanski *)
(* recenzent: Kamil Zwierzchowski *)

let nwd a b =
  let rec pom a b =
    if b = 0 then a
    else pom b (a mod b)
  in pom (max a b) (min a b);;

(* funkcja czy_mozliwe sprawdza, czy dla danych wejsciowych istnieje ciag operacji *)
(* po ktorych w kazdej i-tej szklance bedzie dokladnie y_i wody *)
let czy_mozliwe szklanki =
  if Array.length szklanki = 0 then true
  else
    (* musi istniec przynajmniej jedna szklanka dla ktorej x_i > 0, *)
    (* i jednoczesnie zachodzi y_i = x_i lub y_i = 0 *)
    let pelna_pusta = Array.fold_left (fun a (x, y) -> (a || ((x = y || y = 0) && x <> 0))) false szklanki in
    if pelna_pusta = false then false
    else
      (* najmniejsza ilosc wody jaka mozemy dowoli przelewac to nwd (x_1, x_2, ..., x_n), *)
      (* wiec kazde y_i musi byc przez to podzielne *)
      let pom = Array.fold_left (fun a (x, _) -> nwd a x) 0 szklanki in
      match pom with
      | 0 -> true
      | _ -> Array.fold_left (fun a (_, y) -> (a && (y mod pom = 0))) true szklanki;;

(* funkcja sprawdz odpowiada na pytanie czy przekazany stan wody w szklankach jest rowny docelowemu *)
let sprawdz stan szklanki =
  fst (Array.fold_left (fun (a, b) (_, y) -> (a && stan.(b) = y, b + 1)) (true, 0) szklanki);;

(* funkcja dodaj sprawdza czy w podanej tablicy hashowej istnieje juz wpisana wartosc dla danego stanu *)
(* - i jezeli nie - to wpisuje (odpowiedz funkcji to czy wpisala nowa wartosc) *)
let dodaj stan hasztab war =
  if Hashtbl.mem hasztab stan = false then (
    Hashtbl.add hasztab stan war;
    true;
  ) else false;;

(* funkcja dolej wypelnia podana szklanke do pelna i zwraca stan wody w naczyniach po tej operacji *)
let dolej stan nr szklanki =
  let pom = Array.copy stan in (
    pom.(nr) <- fst (szklanki.(nr));
    pom;
  );;

(* funkcja wylej wylewa cala wode z danej szklanki i zwraca stan wody w naczyniach po tej operacji *)
let wylej stan nr =
  let pom = Array.copy stan in (
    pom.(nr) <- 0;
    pom;
  );;

(* funkcja przelej przelewa maksymalna liczbe wody jaka sie da z zadanej szklanki do innej *)
(* i zwraca stan wody w naczyniach po tej operacji *)
let przelej stan nr_z nr_do szklanki =
  let pom = Array.copy stan in
  let wolne = (fst szklanki.(nr_do)) - stan.(nr_do) in
  let ile = stan.(nr_z) in
  if ile <= wolne then (
    pom.(nr_z) <- 0;
    pom.(nr_do) <- pom.(nr_do) + ile;
  ) else (
    pom.(nr_z) <- pom.(nr_z) - wolne;
    pom.(nr_do) <- fst szklanki.(nr_do);
  ); pom;;

(* funkcja przelewanka dziala jak algorytm BFS dla stanow wody w szklankach, *)
(* dla ktorych w tablicy hashowej trzymana jest minimalna liczba operacji potrzebna *)
(* aby ten stan uzyskac *)
let przelewanka szklanki = 
  let n = Array.length szklanki in
  let stan = Array.make n 0 in
  if sprawdz stan szklanki = true then 0
  else 
    if czy_mozliwe szklanki = false then (-1)
    else
      let hasztab = Hashtbl.create 1002173 in
      let kol = Queue.create () in
      let odp = ref (-1) in
      Hashtbl.add hasztab stan 0;
      Queue.push stan kol;
      while (!odp = (-1) && not (Queue.is_empty kol)) do
        let pom_stan = Queue.pop kol in
        let odl = Hashtbl.find hasztab pom_stan + 1 in
        for i = 0 to (n - 1) do
          let dolano = dolej pom_stan i szklanki in
          let wylano = wylej pom_stan i in
          if sprawdz dolano szklanki = true then odp := odl;
          if sprawdz wylano szklanki = true then odp := odl;
          let ok1 = dodaj dolano hasztab odl in
          let ok2 = dodaj wylano hasztab odl in
          if ok1 = true then Queue.push dolano kol;
          if ok2 = true then Queue.push wylano kol;
          for j = 0 to (n - 1) do
            if i <> j then (
              let przelano = przelej pom_stan i j szklanki in
              if sprawdz przelano szklanki = true then odp := odl;
              let ok = dodaj przelano hasztab odl in
              if ok = true then Queue.push przelano kol;
            )
          done;
        done;
      done;
    !odp;;

(* 
let a = [|(1, 1); (2, 1)|]

let a = [||];;

let a = [|(50, 50); (50, 48); (2, 2)|];;

let a = [|(13, 9); (17, 3); (7, 2); (2, 2)|];;

let a = [|(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (99, 66); (3, 3)|];;

let a = [|(37, 3); (42, 37); (69, 33)|];;

let a = [|(24, 13); (12, 5); (6, 2); (1, 0)|];;

let a = [|(100, 0); (50, 0); (100000, 0); (35, 0)|];;
*)