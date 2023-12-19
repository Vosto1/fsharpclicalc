namespace Calculator.Parser

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Lexer.Lexer
module Parser =
    exception ParseError of string
    let mutable Index = 0

    let lexMatch (token : string * int * Token) (lexeme : string) (expected : TokenType) =
        (second2 (last3 token)) = lexeme
        &&
        (first2 (last3 token)) = expected

    let consume (data : string) (index : int) =
        let token = nextToken (data, index)
        Index <- (middle3 token)
        token

    let peek (data : string) (index : int) = peekToken (data, index)
    let getType (token : string * int * Token) = first2 (last3 token)

    let parse (data : string) =

        let rec Expr () =
            let res1 = Expr2 ()
            let res2 = Expr12 ()
            match res2 with
            | Nothing -> res1
            | x -> BinaryOperatorExpressionStart(res1, x)
        
        and Expr12 () =
            let token = peek data Index
            let t = getType token
            match t with
            | OP(x) when x = ADD || x = SUB ->
                let operator = x
                let token = consume data Index
                let res1 = Expr2 ()
                let res2 = Expr12 ()
                match (res1, res2) with
                | (x, y) when x = Nothing -> raise (ParseError($"Unexpected end of string at {Index}"))
                | (x, y) when y = Nothing -> BinaryOperatorExpressionEnd(operator, x)
                | (x, y) -> BinaryOperatorExpressionList(operator, x, y)
            | _ -> Nothing
                
        and Expr2 () =
            let res1 = Expr3 ()
            let res2 = Expr22 ()
            match res2 with
            | Nothing -> res1
            | x -> BinaryOperatorExpressionStart(res1, x)
            
        and Expr22 () =
            let token = peek data Index
            let t = getType token
            match t with
            | OP(x) when x = MUL || x = DIV ->
                let operator = x
                let token = consume data Index
                let res1 = Expr3 ()
                let res2 = Expr22 ()
                match (res1, res2) with
                | (x, y) when x = Nothing -> raise (ParseError($"Unexpected end of string at {Index}"))
                | (x, y) when y = Nothing -> BinaryOperatorExpressionEnd(operator, x)
                | (x, y) -> BinaryOperatorExpressionList(operator, x, y)
            | _ -> Nothing
        
        and Expr3 () =
            let token = peek data Index
            let t = getType token
            match t with
            | NUM ->
                let token = consume data Index
                Number(int (second2 (last3 token)))
            | OP(x) ->
                let token = consume data Index
                match x with
                | SUB -> ();
                | _ -> raise (ParseError($"Unexpected symbol at {Index} expected SUB \"-\" found {x.ToString()}"))
                let res = Expr3 ()
                UnaryOperatorExpression(UMINUS, res)
            | SEP ->
                let token = consume data Index
                let res = Expr ();
                let token = consume data Index
                if not (lexMatch token ")" SEP)
                then raise (ParseError($"Unexpected symbol at {Index} expected SEP \")\" found {first2 (last3 token)}"))
                else ParanthesisedExpression(res)
            | _ -> raise (ParseError($"Unexpected end of string at {Index}"))
                
        // run
        Index <- 0
        let mutable success = true
        let mutable result = Nothing
        try
            result <- Expr ()
        with
            | ParseError(x) -> printfn "%A" x; success <- false
            | MatchError(x) -> ()
        
        (success, result)
