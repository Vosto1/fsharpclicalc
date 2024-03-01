namespace Calculator.Parser

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Lexer.Lexer
module Parser =
    exception ParseError of string

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

        let rec Expr (index : int) : Expression * int =
            let res1 = Expr2 index
            let res2 = Expr12 (second2 res1)
            match res2 with
            | (Nothing, x) -> (first2 res1, x)
            | (x, y) -> (BinaryOperatorExpressionStart(first2 res1, x), y)
        
        and Expr12 (index : int) : Expression * int =
            let token = peek data index
            let t = getType token
            match t with
            | OP(x) when x = ADD || x = SUB ->
                let operator = x
                let _, newIndex = consume data index
                let res1 = Expr2 newIndex
                let res2 = Expr12 (second2 res1)
                match (res1, res2) with
                | ((x, _), (_, w)) when x = Nothing -> raise (ParseError($"Unexpected end of string at {w}"))
                | ((x, _), (z, w)) when z = Nothing -> (BinaryOperatorExpressionEnd(operator, x), w)
                | ((x, _), (z, w)) -> (BinaryOperatorExpressionList(operator, x, z), w)
            | _ -> (Nothing, index)
                
        and Expr2 (index : int) : Expression * int =
            let res1 = Expr3 index
            let res2 = Expr22 (second2 res1)
            match res2 with
            | (Nothing, x) -> (first2 res1, x)
            | (x, y) -> (BinaryOperatorExpressionStart(first2 res1, x), y)
            
        and Expr22 (index : int) : Expression * int =
            let token = peek data index
            let t = getType token
            match t with
            | OP(x) when x = MUL || x = DIV ->
                let operator = x
                let _, newIndex = consume data index
                let res1 = Expr3 newIndex
                let res2 = Expr22 (second2 res1)
                match (res1, res2) with
                | ((x, _), (_, w)) when x = Nothing -> raise (ParseError($"Unexpected end of string at {w}"))
                | ((x, _), (z, w)) when z = Nothing -> (BinaryOperatorExpressionEnd(operator, x), w)
                | ((x, _), (z, w)) -> (BinaryOperatorExpressionList(operator, x, z), w)
            | _ -> (Nothing, index)
        
        and Expr3 (index : int) : Expression * int =
            let token = peek data index
            let t = getType token
            match t with
            | NUM ->
                let token, newIndex = consume data index
                (Number(int (second2 (last3 token))), newIndex)
            | OP(x) ->
                let _, newIndex = consume data index
                // TODO: to validate fn
                match x with
                | SUB -> ();
                | _ -> raise (ParseError($"Unexpected symbol at {newIndex} expected SUB \"-\" found {x.ToString()}"))
                let res = Expr3 newIndex
                (UnaryOperatorExpression(UMINUS, first2 res), second2 res)
            | SEP ->
                let _, newIndex = consume data index
                let res = Expr newIndex
                let token, nextIndex = consume data (second2 res)
                if not (lexMatch token ")" SEP)
                then raise (ParseError($"Unexpected symbol at {nextIndex} expected SEP \")\" found {first2 (last3 token)}"))
                else (ParanthesisedExpression(first2 res), nextIndex)
            | _ -> raise (ParseError($"Unexpected end of string at {index}"))

        let mutable success = true
        let mutable result = (Nothing, 0)
        try
            result <- Expr 0
        with
            | ParseError(x) -> printfn "%A" x; success <- false
            | MatchError(x) -> ()
        
        (success, first2 result)
