namespace Calculator.Parser

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Lexer.Lexer
open Calculator.Core.Result
module Parser =
    let lexMatch (token : string * int * Token) (lexeme : string) (expected : TokenType) =
        (second2 (last3 token)) = lexeme
        &&
        (first2 (last3 token)) = expected

    let consume (data : string) (index : int) =
        let token = nextToken (data, index)
        (token, middle3 token)

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
            let token = peek data index
            let t = getType token
            match t with
            | OP(x) when x = ADD || x = SUB ->
                let operator = x
                let _, newIndex = consume data index
                Bind (Expr2 newIndex) (fun res1 -> 
                    Bind (Expr12 (second2 res1)) (fun res2 -> 
                        match (res1, res2) with
                        | ((x, _), (_, w)) when x = Nothing -> Error($"Unexpected end of string at {w}")
                        | ((x, _), (z, w)) when z = Nothing -> Success((BinaryOperatorExpressionEnd(operator, x), w))
                        | ((x, _), (z, w)) -> Success((BinaryOperatorExpressionList(operator, x, z), w))))
            | _ -> Success((Nothing, index))
                
        and Expr2 (index : int) : Result<Expression * int> =
            Bind (Expr3 index) (fun res1 -> 
                Bind (Expr22 (second2 res1)) (fun res2 ->
                    match res2 with
                    | (Nothing, x) -> Success((first2 res1, x))
                    | (x, y) -> Success((BinaryOperatorExpressionStart(first2 res1, x), y))))
            
        and Expr22 (index : int) : Result<Expression * int> =
            let token = peek data index
            let t = getType token
            match t with
            | OP(x) when x = MUL || x = DIV ->
                let operator = x
                let _, newIndex = consume data index
                Bind (Expr3 newIndex) (fun res1 -> 
                    Bind (Expr22 (second2 res1)) (fun res2 -> 
                        match (res1, res2) with
                        | ((x, _), (_, w)) when x = Nothing -> Error($"Unexpected end of string at {w}")
                        | ((x, _), (z, w)) when z = Nothing -> Success((BinaryOperatorExpressionEnd(operator, x), w))
                        | ((x, _), (z, w)) -> Success((BinaryOperatorExpressionList(operator, x, z), w))))
            | _ -> Success((Nothing, index))
        
        and Expr3 (index : int) : Result<Expression * int> =
            let validate (op : Operator) (index : int) =
                match op with
                | SUB -> Success()
                | _ -> Error($"Unexpected symbol at {index} expected SUB \"-\" found {op.ToString()}")
            let token = peek data index
            let t = getType token
            match t with
            | NUM ->
                let token, newIndex = consume data index
                Success((Number(int (second2 (last3 token))), newIndex))
            | OP(x) ->
                let _, newIndex = consume data index
                Bind (Expr3 newIndex) (fun res -> 
                    Bind (validate x newIndex) (fun _ -> 
                        Success((UnaryOperatorExpression(UMINUS, first2 res), second2 res))))
            | SEP ->
                let _, newIndex = consume data index
                Bind (Expr newIndex) (fun res -> 
                    let token, nextIndex = consume data (second2 res)
                    if not (lexMatch token ")" SEP)
                    then Error($"Unexpected symbol at {nextIndex} expected SEP \")\" found {first2 (last3 token)}")
                    else Success((ParanthesisedExpression(first2 res), nextIndex)))
            | _ -> Error($"Unexpected end of string at {index}")

        Expr 0
