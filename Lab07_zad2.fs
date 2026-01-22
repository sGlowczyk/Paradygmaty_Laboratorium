open System

// ====== KLASA BANKACCOUNT ======
type BankAccount(accountNumber: string, initialBalance: decimal) =
    let mutable balance = initialBalance

    member _.AccountNumber = accountNumber
    member _.Balance = balance

    member this.Deposit(amount: decimal) =
        if amount <= 0m then invalidArg "amount" "Kwota wpłaty musi być dodatnia."
        balance <- balance + amount
        printfn "Wpłata %.2fm na konto %s. Nowe saldo: %.2fm" amount this.AccountNumber balance

    member this.Withdraw(amount: decimal) =
        if amount <= 0m then invalidArg "amount" "Kwota wypłaty musi być dodatnia."
        elif amount > balance then invalidOp "Brak środków na koncie."
        else
            balance <- balance - amount
            printfn "Wypłata %.2fm z konta %s. Nowe saldo: %.2fm" amount this.AccountNumber balance


// ====== KLASA BANK (CRUD) ======
type Bank() =
    let mutable accounts : Map<string, BankAccount> = Map.empty

    member _.CreateAccount(number: string, initialBalance: decimal) =
        if accounts.ContainsKey number then invalidOp "Konto o takim numerze już istnieje."
        let acc = BankAccount(number, initialBalance)
        accounts <- accounts.Add(number, acc)
        printfn "Utworzono konto %s z saldem %.2fm." number initialBalance
        acc

    member _.TryGetAccount(number: string) =
        accounts |> Map.tryFind number

    member _.UpdateAccount(acc: BankAccount) =
        accounts <- accounts.Add(acc.AccountNumber, acc)

    member _.DeleteAccount(number: string) =
        match accounts |> Map.tryFind number with
        | Some _ ->
            accounts <- accounts.Remove number
            printfn "Usunięto konto %s." number
        | None ->
            printfn "Konto %s nie istnieje." number

    member _.ListAccounts() =
        printfn "\nLista kont w banku:"
        if accounts.IsEmpty then
            printfn "  (brak kont)"
        else
            accounts
            |> Map.iter (fun _ acc ->
                printfn "  - %s: saldo %.2fm" acc.AccountNumber acc.Balance)
        printfn ""


// ====== HELPERY WEJŚCIA ======
let readLineWith (prompt: string) =
    printf "%s" prompt
    Console.ReadLine()

let readDecimalOrZero (prompt: string) =
    let s = readLineWith prompt
    match Decimal.TryParse s with
    | true, v -> v
    | _ ->
        printfn "Niepoprawna kwota, przyjmuję 0."
        0m

let menu () =
    printfn "===== SYSTEM BANKOWY ====="
    printfn "1 - Utwórz konto"
    printfn "2 - Pokaż konto"
    printfn "3 - Wpłata"
    printfn "4 - Wypłata"
    printfn "5 - Lista wszystkich kont"
    printfn "6 - Usuń konto"
    printfn "0 - Wyjście"
    readLineWith "Wybór: "


// ====== PROGRAM GŁÓWNY ======
[<EntryPoint>]
let main _ =
    let bank = Bank()
    let mutable running = true

    while running do
        match menu() with
        | "1" ->
            let number  = readLineWith "Numer konta: "
            let initial = readDecimalOrZero "Saldo początkowe: "
            try
                bank.CreateAccount(number, initial) |> ignore
            with ex ->
                printfn "Błąd: %s" ex.Message

        | "2" ->
            let number = readLineWith "Numer konta: "
            match bank.TryGetAccount number with
            | Some acc ->
                printfn "Konto %s, saldo: %.2fm" acc.AccountNumber acc.Balance
            | None ->
                printfn "Konto %s nie istnieje." number

        | "3" ->
            let number = readLineWith "Numer konta: "
            let amount = readDecimalOrZero "Kwota wpłaty: "
            match bank.TryGetAccount number with
            | Some acc ->
                try
                    acc.Deposit(amount)
                    bank.UpdateAccount(acc)
                with ex ->
                    printfn "Błąd: %s" ex.Message
            | None ->
                printfn "Konto %s nie istnieje." number

        | "4" ->
            let number = readLineWith "Numer konta: "
            let amount = readDecimalOrZero "Kwota wypłaty: "
            match bank.TryGetAccount number with
            | Some acc ->
                try
                    acc.Withdraw(amount)
                    bank.UpdateAccount(acc)
                with ex ->
                    printfn "Błąd: %s" ex.Message
            | None ->
                printfn "Konto %s nie istnieje." number

        | "5" ->
            bank.ListAccounts()

        | "6" ->
            let number = readLineWith "Numer konta do usunięcia: "
            bank.DeleteAccount(number)

        | "0" ->
            running <- false
            printfn "Koniec programu."

        | _ ->
            printfn "Nieznana opcja, spróbuj ponownie."

        printfn ""
    0