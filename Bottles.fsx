[<Measure>]
type gal

open Microsoft.FSharp.Math

type Bottle = {capacity: int<gal>; filled: int<gal>}


let fillUp bottle = 
    match bottle.filled with
        | f when f <= bottle.capacity -> {capacity = bottle.capacity; filled = bottle.capacity}
        | _ -> bottle

let empty bottle = {capacity = bottle.capacity; filled = 0<gal>}

let isFull bottle = bottle.capacity = bottle.filled

type state = {left:Bottle; right:Bottle}

let matches state goal = 
    state.left.filled = goal || state.right.filled = goal

type Direction =
    Left | Right
let min (x:int<'u>) (y:int<'u>) =
    let diff = x - y
    match diff with
        | d when d < 0<_> -> x
        | _ -> y

let pour dir state = 
    let give = match dir with 
                |Left -> state.left
                |Right -> state.right
    let recv = match dir with 
                |Left -> state.right
                |Right -> state.left
    let diff = give.filled - recv.filled
    
    let removedQuantity = give.filled - (recv.capacity - recv.filled)
    let rhs = (recv.capacity - recv.filled)
    let addedQuantity = min removedQuantity rhs  
    let updatedRecv = {capacity = recv.capacity;filled = recv.filled + addedQuantity}
    let updatedGive = {capacity = give.capacity; filled = give.filled - removedQuantity}
    if updatedGive.filled < 0<gal> || updatedRecv.filled < 0<gal> then state else 
    match dir with 
        | Left -> {left = updatedGive; right = updatedRecv}
        | Right -> {left = updatedRecv; right = updatedGive}

let start = {left = {capacity=5<gal>; filled = 4<gal>}; right = {capacity=3<gal>; filled=1<gal>}}

let update = pour Left start

let fill dir state = 
    match dir with
        | Left -> {left = fillUp state.left; right = state.right}
        | Right -> {left = state.left; right = fillUp state.right}
let dumpOut bottle =
    {capacity = bottle.capacity; filled = 0<gal>}
let dump dir state = 
    match dir with
        | Left -> {left = dumpOut state.left; right = state.right}
        | Right -> {left = state.left; right = dumpOut state.right}

let bigUpdate = start |> (pour Left) |> (fill Left) |> (pour Right) |> (dump Left) 

type Action = Direction -> state -> state

//Set up the DFS
let visited : Set<state> = Set.ofList []

//Possible actions
//let possibleActions : List<Action> = [(pour Left); (pour Right); (fill Left); (fill Right); (dump Left); (dump Right)]

let path : List<Action> = []

type PlanStep = {action: Action; which : Direction}

let possibleSteps = [{action=pour; which = Left}; {action=pour; which=Right}; {action=fill; which=Left}; {action=fill; which=Right}; {action=dump;which=Left}; {action=dump;which=Right}]

type Plan = List<PlanStep>

type PlanResult = 
    | Success of Plan
    | Failure

let rec solve start goal (visited : Set<state>) (path : Plan) =
    if matches start goal then (Success path) else
    let updatedVisit = visited.Add start

    let rec attemptSolve (actions : List<PlanStep>) = 
        match actions with
            | x::xs -> 
                let action = (x.action x.which)
                let neigh = action start
                let updatedPath = path @ [x]
                if not (updatedVisit.Contains neigh) then
                    let res = solve neigh goal updatedVisit updatedPath 
                    match res with
                        | Success plan -> Success plan
                        | Failure -> attemptSolve xs
                else
                    attemptSolve xs
            | [] -> Failure

    attemptSolve possibleSteps

let planStart = {left = {capacity=5<gal>; filled = 0<gal>}; right = {capacity=3<gal>; filled=0<gal>}}

let desiredRes = 2<gal>

let res = solve planStart desiredRes visited []

let prettyResult res = 
    match res with
        | Success plan -> plan |> List.iter (fun x -> printfn "%A" x)
        | Failure -> printfn "No plan found"

prettyResult res