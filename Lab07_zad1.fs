open System

// ====== KLASA BOOK ======
type Book(title: string, author: string, pages: int) =
    member _.Title  = title
    member _.Author = author
    member _.Pages  = pages
    member _.GetInfo() =
        sprintf "\"%s\" – %s, %d stron" title author pages

// ====== KLASA USER ======
type User(name: string) =
    let mutable borrowedBooks : Book list = []

    member _.Name = name

    member this.BorrowBook(book: Book) =
        borrowedBooks <- book :: borrowedBooks
        printfn "%s wypożyczył(a) książkę: %s" this.Name (book.GetInfo())

    member this.ReturnBook(title: string) =
        let toReturn, remaining =
            borrowedBooks |> List.partition (fun b -> b.Title = title)

        match toReturn with
        | [] ->
            printfn "%s nie ma książki o tytule \"%s\"." this.Name title
        | _ ->
            borrowedBooks <- remaining
            printfn "%s zwrócił(a) książkę \"%s\"." this.Name title

    member this.ListBorrowed() =
        printfn "Książki wypożyczone przez %s:" this.Name
        match borrowedBooks with
        | [] -> printfn "  (brak)"
        | xs -> xs |> List.iter (fun b -> printfn "  - %s" (b.GetInfo()))
        printfn ""

// ====== KLASA LIBRARY ======
type Library(name: string) =
    let mutable available : Book list = []

    member _.Name = name

    member _.AddBook(book: Book) =
        available <- book :: available
        printfn "Dodano: %s" (book.GetInfo())

    member _.RemoveBook(title: string) =
        let removed, rest =
            available |> List.partition (fun b -> b.Title = title)

        match removed with
        | [] ->
            printfn "Brak książki o tytule \"%s\" w bibliotece." title
        | _ ->
            available <- rest
            printfn "Usunięto książkę \"%s\" z biblioteki." title

    member _.TryFind(title: string) =
        available |> List.tryFind (fun b -> b.Title = title)

    member _.ListBooks() =
        printfn "\nKsiążki w bibliotece \"%s\":" name
        match available with
        | [] -> printfn "  (brak)"
        | xs -> xs |> List.iter (fun b -> printfn "  - %s" (b.GetInfo()))
        printfn ""


// ====== I/O helpers ======
let readIntOrZero (prompt: string) =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | true, v  -> v
    | false, _ ->
        printfn "Niepoprawna liczba, przyjmuję 0."
        0

let showMenu () =
    printfn "===== SYSTEM BIBLIOTECZNY ====="
    printfn "1 - Dodaj książkę"
    printfn "2 - Usuń książkę"
    printfn "3 - Wyświetl wszystkie książki"
    printfn "4 - Wypożycz książkę użytkownikowi"
    printfn "5 - Zwróć książkę"
    printfn "6 - Pokaż książki wypożyczone przez użytkownika"
    printfn "0 - Wyjście"
    printf   "Wybór: "
    Console.ReadLine()

let readLineWith (prompt: string) =
    printf "%s" prompt
    Console.ReadLine()

let createBookFromInput () =
    let t = readLineWith "Tytuł: "
    let a = readLineWith "Autor: "
    let p = readIntOrZero "Liczba stron: "
    Book(t, a, p)


// ====== GŁÓWNY PROGRAM ======
[<EntryPoint>]
let main _ =
    let lib = Library("Biblioteka Główna")

    let userName = readLineWith "Podaj imię użytkownika: "
    let user = User(userName)

    let mutable keepGoing = true

    while keepGoing do
        match showMenu() with
        | "1" ->
            lib.AddBook (createBookFromInput())

        | "2" ->
            let title = readLineWith "Podaj tytuł książki do usunięcia: "
            lib.RemoveBook title

        | "3" ->
            lib.ListBooks()

        | "4" ->
            let title = readLineWith "Podaj tytuł książki do wypożyczenia: "
            match lib.TryFind title with
            | Some b ->
                user.BorrowBook b
                lib.RemoveBook b.Title
            | None ->
                printfn "Nie znaleziono książki \"%s\"." title

        | "5" ->
            let title = readLineWith "Podaj tytuł książki do zwrotu: "
            user.ReturnBook title
            // zachowuję to samo działanie jak u Ciebie:
            // po zwrocie pytamy o autora i strony i dodajemy nowy egzemplarz
            let author = readLineWith "Autor (dla ponownego dodania do biblioteki): "
            let pages = readIntOrZero "Liczba stron: "
            lib.AddBook (Book(title, author, pages))

        | "6" ->
            user.ListBorrowed()

        | "0" ->
            keepGoing <- false
            printfn "Koniec programu."

        | _ ->
            printfn "Nieznana opcja, spróbuj ponownie."

        printfn ""
    0
