namespace ILMath

module Func =

    [<Literal>]
    let identityMin = 1e-15
    
    let identity = function
        | x when abs x < identityMin -> 
            0.0
        | x when x - 1. = x - 1. ->
            x
        | _ -> 
            nan

module Matrice =

    let sgn i j = -1. ** float (i + j)

    let isSquareMatrice (matrice: 'T [,]) =
        match Array2D.length1 matrice, Array2D.length2 matrice with
        | m, n when m <> n -> 
            None
        | _ -> 
            matrice
            |> Array2D.length1
            |> Some
    
    let minor (matrice: 'T [,]) notI notJ =

        let withoutRow notI (matrice: 'T [,]) = 
            match notI with
            | 0 -> 
                Array2D.init 
                    (Array2D.length1 matrice - 1)
                    (Array2D.length2 matrice) 
                    (fun i j -> 
                        matrice.[1..,*].[i,j])
            | row when row = Array2D.length1 matrice - 1 ->
                Array2D.init 
                    (Array2D.length1 matrice - 1) 
                    (Array2D.length2 matrice) 
                    (fun i j -> 
                        matrice.[0..row - 1,*].[i,j])
            | row -> 
                Array2D.init 
                    (Array2D.length1 matrice - 1) 
                    (Array2D.length2 matrice)
                    (fun i j -> 
                        [|matrice.[0..row - 1,*] ; matrice.[row..,*]|].[i].[i, j])

        let withoutCol notJ (matrice: 'T [,]) =
            match notJ with
            | 0 -> 
                Array2D.init 
                    (Array2D.length1 matrice) 
                    (Array2D.length2 matrice - 1) 
                    (fun i j -> 
                        matrice.[*, 1..].[i,j])
            | col when col = Array2D.length2 matrice - 1 ->
                Array2D.init 
                    (Array2D.length1 matrice) 
                    (Array2D.length2 matrice - 1) 
                    (fun i j -> 
                        matrice.[*, 0..col - 1].[i,j])
            | col -> 
                Array2D.init 
                    (Array2D.length1 matrice) 
                    (Array2D.length2 matrice - 1) 
                    (fun i j -> 
                        [|matrice.[*, 0..col - 1] ; matrice.[*, col..]|].[j].[i, j])

        matrice
        |> withoutRow notI
        |> withoutCol notJ

    let determinant (matrice: float [,]) = 

        let det (matrice: float [,]) rowCol =
            match rowCol with
            | 1 -> 
                matrice.[0,0]
                |> Some
            | 2 -> 
                matrice.[0,0] * matrice.[1,1] - matrice.[0,1] * matrice.[1,0]
                |> Some
            | m ->
                // ToDO: Check the rows if they are the same or all of the elements are zero
                // ToDo: Reorder rows of the matrice so zeros can help the calculation at the next step
                // ToDo: Convert the matrice to the triangular matrice
                // ToDo: multiply elements in the main diagonal
                None

        matrice
        |> isSquareMatrice 
        |> Option.bind 
            (det matrice)
        
    let cofactor (matrice: float [,]) i j = 

        let minorij = minor matrice i j

        minorij
        |> determinant
        |> Option.map 
            ((*) (sgn i j))   