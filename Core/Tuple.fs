namespace Calculator.Core

module Tuple =
    let first2 m =
        match m with
        | (x, _) -> x

    let second2 m =
        match m with
        | (_, y) -> y

    let first3 m =
        match m with
        | (x, _, _) -> x

    let middle3 m =
        match m with
        | (_, y, _) -> y

    let last3 m =
        match m with
        | (_, _, z) -> z