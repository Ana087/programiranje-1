(* 1. NALOGA *)

(* a) *)

let razlika_kvadratov a b =
    (a + b) * (a + b) - ( a * a + b * b)


let uporabi_na_paru f (x, y) =
  (f x , f y)


let rec ponovi_seznam n sez =
  if n <= 0 then [] else sez @ (ponovi_seznam (n - 1) sez)


let rec razdeli sez =
  let rec aux_razdeli sez acc1 acc2 =
    match sez with
    | [] -> (acc1, acc2)
    | x :: xs -> if x < 0 then aux_razdeli xs (x :: acc1) acc2 
                else aux_razdeli xs acc1 (x :: acc2)
  in aux_razdeli sez [] []



(* 2. NALOGA *)

type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Empty

let leaf x = Node (Empty, x, Empty)

let monotona_pot tree = tree




(* 3. NALOGA *)

type 'a veriga = | Filter of ('a -> bool) * 'a list * 'a veriga
                  | Ostalo of 'a list


let test = Filter ( (>) 0, [], Filter ( (>) 10, [], Ostalo []))


let rec vstavi a = function
  | Ostalo xs -> Ostalo (a :: xs)
  | Filter (f, xs, veriga) -> if (f a) then Filter (f, a :: xs, veriga)
                              else Filter (f, xs, vstavi a veriga)



let rec vsebuje a sez =
  match sez with
  | [] -> false
  | x :: xs -> if x = a then true else (vsebuje a xs)

let rec poisci a = function
  | Ostalo xs -> vsebuje a xs
  | Filter (f, xs, veriga) -> if (f a) then (vsebuje a xs) else 
                              poisci a veriga



let rec izprazni_filtre = function
  | Ostalo sez -> (Ostalo [], sez)
  | Filter (f, sez, veriga) -> let prazni, vsebina = izprazni_filtre veriga in (Filter (f, [], prazni), sez @ vsebina)



let rec dodaj_filter f veriga =
  let prazni, vsebina = izprazni_filtre veriga in
  let nova_veriga = Filter (f, [], prazni) in
  List.fold_right vstavi vsebina nova_veriga



















