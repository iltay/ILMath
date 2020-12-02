namespace ILMath

open System

// Types
module Matrice =
    let rec det (matrice: float [,]) = 
        let rows = Array2D.length1 matrice
        let columns = Array2D.length2 matrice
        match rows, columns with
        | m, n when m <> n -> 
            None
        | 1, 1 -> 
            matrice.[0,0]
            |> Some
        | 2, 2 -> 
            matrice.[0,0] * matrice.[1,1] - matrice.[0,1] * matrice.[1,0]
            |> Some
        | m, n ->
            let sgn i j = -1. ** float (i + j)
            let matriceWithoutIJ notI notJ =
               Array2D.init<float> (m - 1) (n - 1) 
                        (fun i j -> 
                            match i, j with
                            | i, j when i < notI && j < notJ ->
                                matrice.[i, j]
                            | i, j when i < notI && j = notJ ->
                                matrice.[i,j + 1]
                            | i, j when i < notI && j > notJ ->
                                matrice.[i, j + 1]
                            | i, j when i = notI && j < notJ ->
                                matrice.[i + 1,j]
                            | i, j when i = notI && j = notJ ->
                                matrice.[i + 1,j + 1]
                            | i, j when i = notI && j > notJ ->
                                matrice.[i + 1,j + 1]
                            | i, j when i > notI && j < notJ ->
                                matrice.[i + 1,j]
                            | i, j when i > notI && j = notJ ->
                                matrice.[i + 1,j + 1]
                            | i, j when i > notI && j > notJ ->
                                matrice.[i + 1,j + 1] 
                        )
           
            (matrice
            |> Array2D.mapi (fun i j el -> printfn "%A" (matriceWithoutIJ i j); sgn i j * el * (matriceWithoutIJ i j |> det |> Option.get)))
                .[0,*]
            |> Array.reduce (+)
            |> Some


type Sign =
    | Positive
    | Negative

type Data =
    | Nan
    | Inf of sign: Sign
    | Epsilon
    | Integer of value: int64
    | Real of value: decimal

type Boundary =
    | Boundary of incLow: bool * lower: Data * incUp: bool * upper: Data

// Functions

// Results