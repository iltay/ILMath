// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type Sign =
    | Positive
    | Negative

type Data =
    | Inf of sign: Sign
    | Epsilon
    | Integers of value: int64
    | Rational of numurator:int64 * denominator: int64
    | Real of value: decimal

type Boundary =
    | Boundary of incLow: bool * lower: Data * incUp: bool * upper: Data

let rationalToReal (Rational (a, b)) =
    if b <> 0L then Some ((a / b), a % b)
    else None

Rational(6L,5L) |> rationalToReal

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code