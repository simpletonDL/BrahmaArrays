module WorkflowBasics.State.Builder

type State<'state, 'result> =
    State of ('state -> 'result * 'state)

let runState (State s) = s

let execState state = fst << runState state

let evalState state = snd << runState state

let ignoreResult (state: State<'s, 'a>) =
    State <| fun st ->
        let _, new_st = runState state st
        ((), new_st)
        
let getState = State (fun st -> st, st)

let putState st = State (fun _ -> ((), st))

type StateBuilder<'state>() =
    member this.Return (x : 'a) : State<'state, 'a> =
        State <| fun st -> (x, st)
        
    member this.ReturnFrom m =
        m
    
    member this.Bind (m  : State<'state, 'a>,
                      k  : 'a -> State<'state, 'b>) =
        State <| fun st ->
            let (res, st) = runState m st
            let (res', st') = runState <| k res <| st
            (res', st')
    
    member this.Zero () =
        State <| fun st -> (), st
        
    member this.TryWith(tryBlock : State<'state, 'a>,
                        handler  : exn -> State<'state, 'a>) =
         State <| fun st ->
         try
             runState tryBlock st
         with
         | e ->
             runState (handler e) st
