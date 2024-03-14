namespace Calculator.Parser

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Lexer.Lexer
open Calculator.Core.Result
module Parser =
    let lexMatch (token : string * Token) (expected : TokenType) =
        let isOperator expected =
            match expected with
            | OP(x) -> true
            | _ -> false
        let isSeparator expected =
            match expected with
            | SEP(x) -> true
            | _ -> false
        let isFunction expected =
            match expected with
            | FUNC(x) -> true
            | _ -> false
        (first2 (second2 token)) = expected
        &&
        match (first2 (second2 token)) with
        | x when isSeparator x ->
            match (expected, x) with
            | (SEP(y), SEP(z)) -> y = z
            | _ -> false
        | x when isOperator x ->
            match (expected, x) with
            | (OP(y), OP(z)) -> y = z
            | _ -> false
        | x when isFunction x ->
            match (expected, x) with
            | (FUNC(y), FUNC(z)) -> y = z
            | _ -> false
        | _ -> false

    let consume (data : string) (index : int) =
        Bind (nextToken (data, index)) (fun token -> Success(((first3 token, last3 token), middle3 token)))
        

    let peek (data : string) (index : int) = peekToken (data, index)
    let getType (token : string * int * Token) = first2 (last3 token)

    let parse (data : string) =

        let rec Expr (index : int) : Result<Expression * int> =
            Bind (Expr2 index) (fun res1 -> 
                Bind (Expr12 (second2 res1)) (fun res2 -> 
                    match res2 with
                    | (Nothing, x) -> Success((first2 res1, x))
                    | (x, y) -> Success((BinaryOperatorExpressionStart(first2 res1, x), y))))
        
        and Expr12 (index : int) : Result<Expression * int> =
            Bind (peek data index) (fun token -> 
                let t = getType token
                match t with
                | OP(x) when x = ADD || x = SUB ->
                    let operator = x
                    Bind (consume data index) (fun (_, newIndex) -> 
                        Bind (Expr2 newIndex) (fun res1 -> 
                            Bind (Expr12 (second2 res1)) (fun res2 -> 
                                match (res1, res2) with
                                | ((x, _), (_, w)) when x = Nothing -> Error($"Unexpected end of string at {w}")
                                | ((x, _), (z, w)) when z = Nothing -> Success((BinaryOperatorExpressionEnd(operator, x), w))
                                | ((x, _), (z, w)) -> Success((BinaryOperatorExpressionList(operator, x, z), w)))))
                | _ -> Success((Nothing, index)))
                
        and Expr2 (index : int) : Result<Expression * int> =
            Bind (Expr3 index) (fun res1 -> 
                Bind (Expr22 (second2 res1)) (fun res2 ->
                    match res2 with
                    | (Nothing, x) -> Success((first2 res1, x))
                    | (x, y) -> Success((BinaryOperatorExpressionStart(first2 res1, x), y))))
            
        and Expr22 (index : int) : Result<Expression * int> =
            Bind (peek data index) (fun token -> 
                let t = getType token
                match t with
                | OP(x) when x = MUL || x = DIV ->
                    let operator = x
                    Bind (consume data index) (fun (_, newIndex) ->
                        Bind (Expr3 newIndex) (fun res1 -> 
                            Bind (Expr22 (second2 res1)) (fun res2 -> 
                                match (res1, res2) with
                                | ((x, _), (_, w)) when x = Nothing -> Error($"Unexpected end of string at {w}")
                                | ((x, _), (z, w)) when z = Nothing -> Success((BinaryOperatorExpressionEnd(operator, x), w))
                                | ((x, _), (z, w)) -> Success((BinaryOperatorExpressionList(operator, x, z), w)))))
                | _ -> Success((Nothing, index)))

        and Expr3 (index : int) : Result<Expression * int> =
            Bind (Expr4 index) (fun res1 ->
                Bind (Expr33 (second2 res1)) (fun res2 ->
                    match res2 with
                    | (Nothing, x) -> Success((first2 res1, x))
                    | (x, y) -> Success((BinaryOperatorExpressionStart(first2 res1, x), y))))

        and Expr33 (index : int) : Result<Expression * int> =
            Bind (peek data index) (fun token -> 
                let t = getType token
                match t with
                | OP(x) ->
                    match x with
                    | POWER ->
                        let operator = x
                        Bind (consume data index) (fun (_, newIndex) ->
                            Bind (Expr4 newIndex) (fun res1 ->
                                Bind (Expr33 (second2 res1)) (fun res2 -> 
                                    match (res1, res2) with
                                    | ((x, _), (_, w)) when x = Nothing -> Error($"Unexpected end of string at {w}")
                                    | ((x, _), (z, w)) when z = Nothing -> Success((BinaryOperatorExpressionEnd(operator, x), w))
                                    | ((x, _), (z, w)) -> Success((BinaryOperatorExpressionList(operator, x, z), w)))))
                    | _ -> Success((Nothing, index))
                | _ -> Success((Nothing, index)))
        
        and Expr4 (index : int) : Result<Expression * int> =
            let validate (op : Operator) (index : int) =
                match op with
                | SUB -> Success()
                | _ -> Error($"Unexpected symbol at {index} expected SUB \"-\" found {op.ToString()}")
            Bind (peek data index) (fun token ->
                let t = getType token
                match t with
                | NUM ->
                    Bind (consume data index) (fun (token, newIndex) ->
                        Success((Number(int (second2 (second2 token))), newIndex)))
                | OP(x) ->
                    Bind (consume data index) (fun (_, newIndex) ->
                        Bind (Expr3 newIndex) (fun res -> 
                            Bind (validate x newIndex) (fun _ -> 
                                Success((UnaryOperatorExpression(UMINUS, first2 res), second2 res)))))
                | SEP(x) ->
                    match x with
                    | LPAR ->
                        Bind (consume data index) (fun (_, newIndex) ->
                            Bind (Expr newIndex) (fun res -> 
                                Bind (consume data (second2 res)) (fun (token, nextIndex) ->
                                    if not (lexMatch token (SEP(RPAR)))
                                    then Error($"Unexpected symbol at {second2 res} expected SEP \")\" found {first2 (second2 token)}")
                                    else Success((ParanthesisedExpression(first2 res), nextIndex)))))
                    | _ -> Error($"Unexpected symbol at {index} expected LPAR \"(\" found {x.ToString()}")
                | FUNC(x) ->
                    match x with
                    | ROOT -> 
                        Bind (consume data index) (fun (_, newIndex) ->
                            Bind (consume data newIndex) (fun (token, nextIndex) ->
                                if not (lexMatch token (SEP(LPAR)))
                                then Error($"Unexpected symbol at {newIndex} expected LPAR \"(\" found {second2 token}")
                                else
                                    Bind (Expr nextIndex) (fun (res, nextIndex1) ->
                                        match res with
                                        | Nothing -> Error($"Unexpected empty expression at {nextIndex} expected expression")
                                        | _ ->
                                            Bind (consume data nextIndex1) (fun (token, nextIndex2) ->
                                                if not (lexMatch token (SEP(COMMA)))
                                                then Error($"Unexpected symbol at {nextIndex1} expected COMMA \",\" found {second2 token}")
                                                else
                                                    Bind (Expr nextIndex2) (fun (res2, nextIndex3) ->
                                                        match res2 with
                                                        | Nothing -> Error($"Unexpected empty expression at {nextIndex2} expected expression")
                                                        | _ ->
                                                            Bind (consume data nextIndex3) (fun (token, nextIndex4) ->
                                                                if not (lexMatch token (SEP(RPAR)))
                                                                then Error($"Unexpected symbol at {nextIndex3} expected RPAR \")\" found {second2 token}")
                                                                else Success((FunctionExpression(ROOT, res::res2::[])), nextIndex4)))))))
                    | _ -> Error($"Unexpected symbol at {index} expected SQRT \"sqrt\" found {x.ToString()}")
                | CONST(x) ->
                    match x with
                    | x when x = EULER || x =PI -> 
                        Bind (consume data index) (fun (_, newIndex) -> 
                        Success(ConstantExpression(x), newIndex))
                    | _ -> Error($"Unsupported constant {x.ToString()} at {index}")
                | _ -> Error($"Unexpected end of string at {index}"))
        Expr 0
