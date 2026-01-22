open System

let splitWords (separators: char[]) (text: string) =
    text.Split(separators, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let splitKeepEmpty (separators: char[]) (text: string) =
    text.Split(separators, StringSplitOptions.None)

let readLineWith (prompt: string) =
    printf "%s" prompt
    Console.ReadLine()


// ZADANIE 1

let countWordsAndChars (text: string) =
    let words = splitWords [| ' '; '\t'; '\n'; '\r' |] text
    let charsNoSpaces =
        text
        |> Seq.filter (fun c -> not (Char.IsWhiteSpace c))
        |> Seq.length
    words.Length, charsNoSpaces

let text1 = readLineWith "Podaj tekst: "
let wCount, cCount = countWordsAndChars text1
printfn "Liczba słów: %d" wCount
printfn "Liczba znaków (bez spacji): %d\n" cCount


// ZADANIE 2

let isPalindrome (text: string) =
    let cleaned =
        text.ToLowerInvariant()
        |> Seq.filter (fun c -> not (Char.IsWhiteSpace c))
        |> Seq.toArray
    cleaned = (cleaned |> Array.rev)

let palText = readLineWith "Podaj tekst do sprawdzenia (palindrom): "
printfn "%s\n" (if isPalindrome palText then "Tekst jest palindromem." else "Tekst NIE jest palindromem.")


// ZADANIE 3

let uniqueWords (words: string list) = words |> List.distinct

let line3 = readLineWith "Podaj słowa oddzielone spacjami: "
let words3 = splitWords [| ' '; '\t' |] line3
let uniques = uniqueWords words3

printfn "Unikalne słowa:"
uniques |> List.iter (fun w -> printfn " - %s" w)
printfn ""

// ZADANIE 4

let reformatPerson (line: string) =
    let parts =
        line.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())

    if parts.Length <> 3 then
        failwith $"Niepoprawny format: {line}"

    let firstName = parts[0]
    let lastName  = parts[1]
    let age       = parts[2]
    $"{lastName}, {firstName} ({age} lat)"

printfn "Podawaj dane w formacie: imię; nazwisko; wiek"
printfn "Pusta linia kończy wprowadzanie.\n"

let mutable entries : string list = []
let mutable reading = true

while reading do
    let l = readLineWith "Wpis: "
    if String.IsNullOrWhiteSpace l then
        reading <- false
    else
        entries <- l :: entries

entries <- List.rev entries

printfn "\nWynik w nowym formacie:"
entries |> List.map reformatPerson |> List.iter (printfn "%s")
printfn ""


// ZADANIE 5

let longestWord (text: string) =
    let words =
        splitWords
            [| ' '; '\t'; '\n'; '\r'; '.'; ','; ';'; ':'; '!'; '?' |]
            text

    match words with
    | [] -> None
    | _ ->
        let best = words |> List.maxBy (fun w -> w.Length)
        Some(best, best.Length)

let text5 = readLineWith "Podaj tekst: "
match longestWord text5 with
| Some(word, len) ->
    printfn "Najdłuższe słowo: \"%s\" (długość: %d)\n" word len
| None ->
    printfn "Brak słów w tekście.\n"

// ZADANIE 6

let replaceWord (oldWord: string) (newWord: string) (text: string) =
    let parts = splitKeepEmpty [| ' '; '\t'; '\n'; '\r' |] text
    parts
    |> Array.map (fun token -> if token = oldWord then newWord else token)
    |> fun arr -> String.Join(" ", arr)

let text6 = readLineWith "Podaj tekst: "
let oldW  = readLineWith "Jakie słowo chcesz znaleźć? "
let newW  = readLineWith "Na jakie słowo zamienić? "

let changed = replaceWord oldW newW text6
printfn "\nOryginał:   %s" text6
printfn "Po zamianie: %s\n" changed


