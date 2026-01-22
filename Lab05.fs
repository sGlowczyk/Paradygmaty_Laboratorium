open System
open System.Collections.Generic


// Zadanie 1 — Fibonacci

let rec fib n =
    if n <= 1 then n
    else fib (n - 1) + fib (n - 2)


let fibTail n =
    let rec loop n a b =
        match n with
        | 0 -> a
        | 1 -> b
        | _ -> loop (n - 1) b (a + b)
    loop n 0 1



// Zadanie 2 — Drzewo + wyszukiwanie

type Tree<'T> =
    | Empty
    | Node of 'T * Tree<'T> * Tree<'T>


let rec containsRec value tree =
    match tree with
    | Empty -> false
    | Node(v, l, r) ->
        v = value || containsRec value l || containsRec value r


let containsIter value tree =
    let mutable stack = [ tree ]
    let mutable ok = false

    while (not ok) && (not stack.IsEmpty) do
        match stack.Head with
        | Empty ->
            stack <- stack.Tail
        | Node(v, l, r) ->
            if v = value then ok <- true
            else
                stack <- l :: r :: stack.Tail
    ok



// Zadanie 3 — Permutacje

let rec permutations xs =
    match xs with
    | [] -> [ [] ]
    | _ ->
        xs
        |> List.collect (fun x ->
            let rest = xs |> List.filter ((<>) x)
            permutations rest |> List.map (fun p -> x :: p)
        )


// Zadanie 4 — Wieże Hanoi

let rec hanoi n src dst aux =
    if n > 0 then
        hanoi (n - 1) src aux dst
        printfn "Przenieś dysk %d z %s do %s" n src dst
        hanoi (n - 1) aux dst src


let hanoiIter n =
    let total = pown 2 n - 1

    let diskNumber (move: int) =
        let mutable cnt = 0
        let mutable t = move
        while t % 2 = 0 do
            t <- t / 2
            cnt <- cnt + 1
        cnt + 1

    for i in 1 .. total do
        let d = diskNumber i
        let fromPeg = (i &&& (i - 1)) % 3
        let toPeg   = ((i ||| (i - 1)) + 1) % 3
        printfn "Ruch %d: Dysk %d z kołka %d na %d" i d fromPeg toPeg



// Zadanie 5 — QuickSort

let rec quicksortRec xs =
    match xs with
    | [] -> []
    | pivot :: tail ->
        let smaller = tail |> List.filter (fun x -> x <= pivot)
        let bigger  = tail |> List.filter (fun x -> x > pivot)
        quicksortRec smaller @ [ pivot ] @ quicksortRec bigger

let quicksortIter (arr: 'T array) =
    let stack = Stack<int * int>()
    stack.Push(0, arr.Length - 1)

    while stack.Count > 0 do
        let low, high = stack.Pop()
        if low < high then
            let pivot = arr.[high]
            let mutable i = low - 1

            for j in low .. high - 1 do
                if arr.[j] <= pivot then
                    i <- i + 1
                    let tmp = arr.[i]
                    arr.[i] <- arr.[j]
                    arr.[j] <- tmp

            let tmp = arr.[i + 1]
            arr.[i + 1] <- arr.[high]
            arr.[high] <- tmp

            let p = i + 1
            stack.Push(low, p - 1)
            stack.Push(p + 1, high)
