// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Types
type Sign =
    | Positive
    | Negative

type Data =
    | Nan
    | Inf of sign: Sign
    | Epsilon
    | Integer of value: int64
    | Real of value: decimal

type Boundary<'T> =
    | Boundary of incLow: bool * lower: Data * incUp: bool * upper: Data

// Functions

// Results


[<EntryPoint>]
let main argv =

    0