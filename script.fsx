module Types =
    type Sign =
        | Positive
        | Negative

    type Data =
        | Inf of Sign: Sign
        | Epsilon
        | Integers of Value: int64
        | Real of Value: decimal

    type Boundary =
        | Boundary of Include: bool * Low: Data * Include: bool * Value: Data

    type Calcuations  = 
        | Random of Boundary
        | Matrice of Data [,]
        | Add of Data * Data
        | Subtract of Data * Data
        | Multiply of Data * Data
        | Divide of Data * Data
        | Remainder of Data * Data
        | Abs of Data
        | Floor of Data
        | Ceil of Data
        | Round of Data
        | Power of Data
        | Sqrt of Data
        | Log of Data
        | Ln of Data
        | Exp of Data
        | Factoriel of Data
        | Sin of Data
        | Cos of Data
        | Tan of Data
        | Cot of Data
        | Csc of Data
        | Sec of Data

    type Function =
        | Function of Name: string * Domain: Boundary * Arguments: Data list option
    
module Constants = 
    open type Types.Sign
    open type Types.Data
    let pi = Real(Positive, System.Math.PI |> decimal)