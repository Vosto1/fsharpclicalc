namespace Calculator.Interpreter

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
module Interpreter =
    type NextOperation =
        | Nothing
        | Operator of Operator

    exception EvaluationError of string
    let Evaluate (expression : Expression) =

        let validateOperation (x : float) (operator : Operator) (y : float) =
            match operator with
            | DIV -> if y = 0 then raise (EvaluationError($"Division by zero {x} / {y}.")) else ()
            | _ -> ()
        let executeOperation (x : float) (operator : Operator) (y : float) =
            match operator with
            | ADD -> x + y
            | SUB -> x - y
            | MUL -> x * y
            | DIV -> x / y

        let executeUnaryOperation (operator : Unary) (x : float) =
            match operator with
            | UMINUS -> -x

        let rec eval expr =
            match expr with
            | Number(x) -> (Nothing, x)
            | BinaryOperatorExpressionStart(x, y) ->
                let res1, res2 = eval x
                let res3, res4 = eval y
                match res3 with
                | Operator(o) -> 
                    validateOperation res2 o res4 |> ignore
                    let value = executeOperation res2 o res4
                    (res1, value)
                | Nothing -> raise (EvaluationError($"There must be a next operation for {y}."))
            | BinaryOperatorExpressionList(x, y, z) ->
                let res1, res2 = eval y
                let res3, res4 = eval z
                match res1 with
                | Nothing ->
                    match res3 with
                    | Operator(o) ->
                        let mutable op = SUB
                        match (x, o) with
                        | (SUB, SUB) ->
                            let value = executeOperation res2 ADD res4
                            (Operator(x), value)
                        | (SUB, ADD) -> 
                            let value = executeOperation res2 SUB res4
                            (Operator(x), value)
                        | _ ->
                            validateOperation res2 o res4 |> ignore
                            let value = executeOperation res2 o res4
                            (Operator(x), value)
                    | Nothing -> raise (EvaluationError($"There must be a next operation for {z}"))
                | x -> raise (EvaluationError($"There cannot be a next operation for {y}"))
            | BinaryOperatorExpressionEnd(x, y) ->
                let res = eval y in
                match res with
                | (Nothing, z) -> (Operator(x), z)
                | x -> raise (EvaluationError($"There cannot be a next operation for {y}"))
            | UnaryOperatorExpression(x, y) ->
                let res = eval y
                match res with
                | (Nothing, z) ->
                    let value = executeUnaryOperation x z
                    (Nothing, value)
                | x -> raise (EvaluationError($"There cannot be a next operation for {y}"))
            | ParanthesisedExpression(x) -> eval x
            | x -> raise (EvaluationError($"Unrecognized construct \"{x}\"."))
        
        eval expression