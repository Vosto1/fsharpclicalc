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
            | POWER -> x ** y
        
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
            | FunctionExpression(x, y) -> evaluateFunctionExpression x y
            | ConstantExpression(x) -> evaluateConstantExpression x
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
        
        and evaluateFunctionExpression functionType paramList =
            let evaluateTwoParameterFunctionApplication (application : float -> float -> float) paramList functionType =
                match paramList with | x::y::[] ->
                    Bind (eval x) (fun res ->
                        Bind (eval y) (fun res2 ->
                            match (res, res2) with
                            | ((Nothing, x), (Nothing, y)) -> Success(Nothing, (application x y))
                            | _ -> Error($"Next operation not possible for {functionType.ToString().ToLower()}({x},{y})")))
            let evaluateOneParameterFunctionApplication (application : float -> float) paramList functionType =
                match paramList with | x::[] ->
                    Bind (eval x) (fun res ->
                        match res with
                        | (Nothing, x) -> Success(Nothing, application x)
                        | _ -> Error($"Next operation not possible for {functionType.ToString().ToLower()}({x})"))

            match functionType with
            | ROOT -> evaluateTwoParameterFunctionApplication (fun x y -> executeOperation y POWER (1.0 / x)) paramList ROOT
            | LOG -> evaluateTwoParameterFunctionApplication (fun x y -> System.Math.Log(y, x)) paramList LOG
            | COS -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Cos x) paramList COS
            | SIN -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Sin x) paramList SIN
            | TAN -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Tan x) paramList TAN
            | ARCCOS -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Acos x) paramList ARCCOS
            | ARCSIN -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Asin x) paramList ARCSIN
            | ARCTAN -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Atan x) paramList ARCTAN
            | NL -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Log x) paramList NL
            | ABS -> evaluateOneParameterFunctionApplication (fun x -> System.Math.Abs x) paramList ABS
            | x -> Error($"Function {x} is unsupported")

        and evaluateConstantExpression constantType =
            match constantType with
            | EULER -> Success(Nothing, System.Math.E)
            | PI -> Success(Nothing, System.Math.PI)
        
        Bind (eval expression) (fun res ->
            match res with
            | (Nothing, x) -> Success(x)
            | _ -> Error("Unable to complete evaluation."))