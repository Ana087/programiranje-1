(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = if x * x = y && x >= 0 then true else false

let pack3 a b c = (a, b, c)

let rec sum_if_not f list = 
  let rec aux_sum f list acc =
  match list with
  | [] -> acc
  | x :: xs -> if f x then aux_sum f xs acc else aux_sum f xs (acc + x)
in aux_sum f list 0


let rec inverz list =
  match list with
  | [] -> []
  | x :: xs -> inverz xs @ [x]

let rec uporabi_funkcije list m =
  let rec uporabi_aux list m acc =
    match list with
    | [] -> inverz acc
    | x :: xs -> uporabi_aux xs m ([x m] @ acc)
  in uporabi_aux list m [] 

let rec apply list1 list2 =
  let rec apply_aux list1 list2 acc = 
    match (list1, list2) with
    | (_, []) -> inverz acc
    | (_, x :: xs) -> apply_aux list1 xs ([(uporabi_funkcije list1 x)] @ acc)
  in apply_aux list1 list2 []

(* Inverz ne dela, funkcija brez inverza deluje.*)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
  | Predavanja
  | Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje : int}

type urnik =
  | Srecanje of list * list

let vaje = {predmet = "Analiza2a"; vrsta = Vaje; trajanje = 3}

let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}



let urnik_profesor = [{predmet = "Programiranje 1"; vrsta = Vaje; trajanje = 2}; {predmet = "Analiza 1"; vrsta = Predavanja; trajanje = 1}; {predmet = "Algebra"; vrsta = Vaje; trajanje = 1}]

let je_preobremenjen () = failwith "dopolni me"

let bogastvo () = failwith "dopolni me"
