open System

// --- DEFINICJA STRUKTURY DANYCH ---
type LinkedList<'T> =
    | Empty
    | Node of 'T * LinkedList<'T>

// Typ pomocniczy do zadania 6
type SearchResult =
    | Index of int
    | NotFound

module LinkedListModule =

    // zadanie 1: Tworzenie listy łączonej z listy standardowej
    let rec fromList (xs: 'T list) =
        match xs with
        | [] -> Empty
        | h :: t -> Node(h, fromList t)

    // Wyświetlanie listy
    let rec printList ll =
        match ll with
        | Empty -> printf "Empty"
        | Node(v, next) ->
            printf "%A -> " v
            printList next

    // zadanie 2: Sumowanie elementów (int)
    let sumList ll =
        let rec go acc l =
            match l with
            | Empty -> acc
            | Node(v, next) -> go (acc + v) next
        go 0 ll

    // zadanie 3: Min/Max
    let findMinMax ll =
        match ll with
        | Empty -> failwith "Lista jest pusta"
        | Node(h, t) ->
            let rec go mn mx l =
                match l with
                | Empty -> mn, mx
                | Node(v, next) -> go (min mn v) (max mx v) next
            go h h t

    // zadanie 4: Odwracanie (ogonowo)
    let reverse ll =
        let rec go acc l =
            match l with
            | Empty -> acc
            | Node(v, next) -> go (Node(v, acc)) next
        go Empty ll

    // zadanie 5: Czy zawiera
    let rec contains value ll =
        match ll with
        | Empty -> false
        | Node(v, next) -> (v = value) || contains value next

    // zadanie 6: Indeks elementu
    let findIndex value ll =
        let rec go i l =
            match l with
            | Empty -> NotFound
            | Node(v, next) ->
                if v = value then Index i
                else go (i + 1) next
        go 0 ll

    // zadanie 7: Liczba wystąpień
    let countOccurrences value ll =
        let rec go acc l =
            match l with
            | Empty -> acc
            | Node(v, next) ->
                let acc' = if v = value then acc + 1 else acc
                go acc' next
        go 0 ll

    // zadanie 8: Łączenie list
    let rec append l1 l2 =
        match l1 with
        | Empty -> l2
        | Node(v, next) -> Node(v, append next l2)

    // zadanie 9: Porównanie list element po elemencie
    let compareLists l1 l2 =
        let rec go a b acc =
            match a, b with
            | Empty, Empty -> List.rev acc
            | Node(v1, n1), Node(v2, n2) -> go n1 n2 ((v1 > v2) :: acc)
            | _ -> failwith "Listy mają różną długość"
        go l1 l2 []

    // zadanie 10: Filtr
    let filter predicate ll =
        let rec go acc l =
            match l with
            | Empty -> reverse acc
            | Node(v, next) ->
                if predicate v then go (Node(v, acc)) next
                else go acc next
        go Empty ll

    // zadanie 11: Usuwanie duplikatów
    let distinct ll =
        let rec go seen acc l =
            match l with
            | Empty -> reverse acc
            | Node(v, next) ->
                if List.contains v seen then go seen acc next
                else go (v :: seen) (Node(v, acc)) next
        go [] Empty ll

    // zadanie 12: Partition
    let partition predicate ll =
        let rec go yes no l =
            match l with
            | Empty -> reverse yes, reverse no
            | Node(v, next) ->
                if predicate v then go (Node(v, yes)) no next
                else go yes (Node(v, no)) next
        go Empty Empty ll


// --- PROGRAM GŁÓWNY (INTERAKCJA) ---
let readIntsFromLine () =
    Console.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map int

[<EntryPoint>]
let main _ =

    printfn "Podaj liczby oddzielone spacjami (np. 1 2 3 2 1):"
    let numbers = readIntsFromLine()
    let myLL = LinkedListModule.fromList numbers

    printf "\nTwoja lista: "
    LinkedListModule.printList myLL
    printfn ""

    // Demonstracja wybranych zadań
    printfn "Suma (zad 2): %d" (LinkedListModule.sumList myLL)

    let mn, mx = LinkedListModule.findMinMax myLL
    printfn "Min/Max (zad 3): %d / %d" mn mx

    printf "Odwrócona (zad 4): "
    LinkedListModule.printList (LinkedListModule.reverse myLL)
    printfn ""

    printf "Podaj liczbę do sprawdzenia: "
    let searchVal = int (Console.ReadLine())

    // zadanie 6 w praktyce
    match LinkedListModule.findIndex searchVal myLL with
    | Index i -> printfn "Znaleziono na indeksie: %d" i
    | NotFound -> printfn "Nie znaleziono elementu."

    // zadanie 11: Unikalne elementy
    printf "Lista bez duplikatów (zad 11): "
    LinkedListModule.printList (LinkedListModule.distinct myLL)
    printfn ""

    printfn "\nKoniec programu. Naciśnij dowolny klawisz..."
    Console.ReadKey() |> ignore
    0
