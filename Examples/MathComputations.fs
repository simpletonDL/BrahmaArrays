module Examples.MathComputations

open WorkflowBasics.State.Builder

type Log = Log of string list

module Log =
    let empty () = Log [] 
    
    let append log s =
        match log with
        | Log xs -> Log <| s :: xs

let computeSum x y =
    State <|
        fun (log : Log) ->
            (x + y, Log.append log <| sprintf "%i + %i" x y)

let computeDiff x y =
    State <|
        fun (log : Log) -> 
            (x - y, Log.append log <| sprintf "%i - %i" x y)

let computeSquare x =
    State <|
        fun (log : Log) -> 
            (x * x, Log.append log <| sprintf "%i^2" x)


let someWorkflowExample () =
    let st =
        StateBuilder() {
            let! st = getState
            printfn "Init state: %A" st
            do! putState <| Log ["Make initial state"]
            let! x = computeSum 39 3
            let! y = computeSquare 7
            do! computeDiff 23 11 |> ignoreResult
            return x + y
        }
    runState st <| Log.empty () |> printfn "%A"
