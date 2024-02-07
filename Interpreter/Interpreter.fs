namespace Calculator.Interpreter

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Core.Result
module Interpreter =
    type NextOperation =
        | Nothing
        | Operator of Operator

    let Evaluate (expression : Expression) =

        let validateOperation (x : float) (operator : Operator) (y : float) =
            match operator with
            | DIV when y = 0 -> Error($"Division by zero {x} / {y}.")
            | _ -> Success()

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
            | Number(x) -> Success((Nothing, x))
            | BinaryOperatorExpressionStart(x, y) -> evaluateBinaryOperatorExpressionStart x y
            | BinaryOperatorExpressionList(x, y, z) -> evaluateBinaryOperatorExpressionList x y z
            | BinaryOperatorExpressionEnd(x, y) -> evaluateBinaryOperatorExpressionEnd x y
            | UnaryOperatorExpression(x, y) -> evaluateUnaryOperatorExpression x y
            | ParanthesisedExpression(x) -> eval x
            | x -> Error($"Unrecognized construct \"{x}\".")

        and evaluateBinaryOperatorExpressionStart expr1 expr2 =
            Bind (eval expr1) (fun (res1, res2) -> 
                Bind (eval expr2) (fun (res3, res4) -> 
                    match res3 with
                    | Operator(o) ->
                        Bind (validateOperation res2 o res4) (fun _ ->
                        let value = executeOperation res2 o res4
                        Success((res1, value)))
                    | Nothing -> Error($"There must be a next operation for {expr2}.")))

        and evaluateBinaryOperatorExpressionList expr1 expr2 expr3 =
            Bind (eval expr2) (fun (res1, res2) -> 
                Bind (eval expr3) (fun (res3, res4) -> 
                    match res1 with
                    | Nothing ->
                        match res3 with
                        | Operator(o) ->
                            match (expr1, o) with
                            | (SUB, SUB) ->
                                let value = executeOperation res2 ADD res4
                                Success((Operator(expr1), value))
                            | (SUB, ADD) ->
                                let value = executeOperation res2 SUB res4
                                Success((Operator(expr1), value))
                            | _ ->
                                validateOperation res2 o res4 |> ignore
                                let value = executeOperation res2 o res4
                                Success((Operator(expr1), value))
                        | Nothing -> Error($"There must be a next operation for {expr3}")
                    | x -> Error($"There cannot be a next operation for {expr2}")))
        
        and evaluateBinaryOperatorExpressionEnd expr1 expr2 =
            Bind (eval expr2) (fun res ->
                match res with
                | (Nothing, z) -> Success((Operator(expr1), z))
                | x -> Error($"There cannot be a next operation for {expr1}"))
        
        and evaluateUnaryOperatorExpression expr1 expr2 =
            Bind (eval expr2) (fun res ->
            match res with
            | (Nothing, z) ->
                let value = executeUnaryOperation expr1 z
                Success((Nothing, value))
            | x -> Error($"There cannot be a next operation for {expr2}"))
        
        Bind (eval expression) (fun res ->
            match res with
            | (Nothing, x) -> Success(x)
            | _ -> Error("Unable to complete evaluation."))