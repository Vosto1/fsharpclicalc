namespace Calculator.Core

module Result =
    type Result<'a> = 
        | Success of 'a
        | Error of string

    let Bind result func =
        match result with
        | Success x -> func x
        | Error x -> Error(x)

    let Reduce result successAction errorAction =
        match result with
        | Success x -> successAction x
        | Error x -> errorAction x