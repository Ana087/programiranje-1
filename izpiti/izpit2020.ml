(*1*)

(*a*)

let rec option_sum a b = function
    | Some a, Some b -> Some (a + b)
    | None, None -> None
    | _ , None -> None
    | None, _ -> None


(*b*)

let prva (a, b) = a

let druga (a, b) = b

let rec twostep_map f g h x =
    (g (prva (f x)) , h (druga (f x)))


(*c*)

let rec ponovitev n x = if n <= 0 then [] else x :: (ponovitev (n - 1) x)

let rec function_repeat f list =
    let rec aux_repeat f acc = function
        | [] -> List.rev acc
        | x :: xs when f x > 0 -> aux_repeat f ((ponovitev (f x) x) @ acc) xs
        | x :: xs -> aux_repeat f acc xs
    in aux_repeat f [] list

(*Funkcija je repno rekurzivna, ker vsebuje akumulator.*)


(*d*)

let rec iterate f pogoj x =
    if pogoj (f x) then f x else iterate f pogoj (f x)



(*2*)

type 'a improved_list =
    | Empty
    | Node of int array * 'a improved_list

let test = Node ( [| 1; 2; 20 |], Node ( [| 17; 19; 20; 30 |] , Node ([| 100 |], Empty)))


(*b*)


let rec count = function
    | Empty -> 0
    | Node (ar, ostalo) -> Array.length ar + (count ostalo)


(*c*)

let rec nth i = function
    | Empty -> None
    | Node (ar, ostalo) -> if i < Array.length ar then Some ar.(i) else nth (i - Array.length ar) ostalo


(*d*)

let rec array_to_list = function
    | Empty -> []
    | Node (ar, ostalo) -> (Array.to_list ar) @ (array_to_list ostalo)
    

let rec is_list_sorted = function
    | [] -> true
    | x :: [] -> true
    | x :: y :: xs -> if x <= y then is_list_sorted (y :: xs) else false

let is_sorted improved_list = 
    is_list_sorted (array_to_list improved_list)


(*e*)

let rec update i x = function
    | Empty -> if i = 0 then Node ([| x |], Empty) else Empty
    | Node (ar, ostalo) -> if Array.length ar > i then Node (ar.(i) <- x, ostalo) else Node ( ar, update (i - (Array.length ar)) x ostalo)