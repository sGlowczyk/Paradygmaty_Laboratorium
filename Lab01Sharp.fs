open System


// ZADANIE 1

printfn "Ile liczb chcesz podać?"
let n = int (Console.ReadLine())

let numbers =
    [ for i in 1 .. n ->
        printf "Podaj liczbę %d: " i
        int (Console.ReadLine()) ]

let sum1 = List.sum numbers
let avg1 = float sum1 / float n
let min1 = List.min numbers
let max1 = List.max numbers

printfn "\nWyniki:"
printfn "Suma: %d" sum1
printfn "Średnia: %.2f" avg1
printfn "Minimum: %d" min1
printfn "Maksimum: %d" max1


// ZADANIE 2

printf "Podaj a: "
let a0 = int (Console.ReadLine())
printf "Podaj b: "
let b0 = int (Console.ReadLine())

let a, b = if a0 <= b0 then a0, b0 else b0, a0
let range = [ a .. b ]

for x in range do
    let parity = if x % 2 = 0 then "parzysta" else "nieparzysta"
    let sign = if x < 0 then "ujemna" elif x = 0 then "zero" else "dodatnia"
    let div3 = if x % 3 = 0 then "tak" else "nie"
    printfn "%d: %s, %s, podzielna przez 3: %s" x parity sign div3

let countBy pred = range |> List.filter pred |> List.length

printfn "\nPODSUMOWANIE:"
printfn "Parzyste: %d" (countBy (fun x -> x % 2 = 0))
printfn "Nieparzyste: %d" (countBy (fun x -> x % 2 <> 0))
printfn "Ujemne: %d" (countBy (fun x -> x < 0))
printfn "Zera: %d" (countBy (fun x -> x = 0))
printfn "Dodatnie: %d" (countBy (fun x -> x > 0))



// ZADANIE 3

printfn "Podaj liczby oddzielone spacją:"
let input =
    Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map int

let positives = input |> List.filter (fun x -> x > 0)
printfn "Liczba dodatnich: %d" positives.Length

let squares = input |> List.map (fun x -> x * x)
printfn "Lista kwadratów: %A" squares

let sumFold = input |> List.fold (fun acc x -> acc + x) 0
printfn "Suma obliczona przez fold: %d" sumFold



// ZADANIE 4

type Student = {
    Imie: string
    Wiek: int
    Ocena: float
}

let students = [
    { Imie = "Jan";   Wiek = 20; Ocena = 4.5 }
    { Imie = "Anna";  Wiek = 21; Ocena = 3.5 }
    { Imie = "Marek"; Wiek = 19; Ocena = 5.0 }
    { Imie = "Ewa";   Wiek = 22; Ocena = 3.0 }
]

let goodStudents =
    students |> List.filter (fun s -> s.Ocena >= 4.0)

printfn "Studenci z wysoką oceną: %A" goodStudents

let olderStudents =
    students |> List.map (fun s -> { s with Wiek = s.Wiek + 1 })

printfn "Studenci rok później: %A" olderStudents
