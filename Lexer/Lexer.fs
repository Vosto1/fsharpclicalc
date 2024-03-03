namespace Calculator.Lexer

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Core.Result
module Lexer = 
    open System.Text.RegularExpressions
    type TokenType = NUM | OP of Operator | SEP | EOF
    type Token = TokenType * string
    let (separator : Regex) = new Regex("\G(\(|\))")
    let (op : Regex) = new Regex("\G(\+|\*|\/|\-)")
    let (num : Regex) = new Regex("\G([0-9]+)")
    
    // get next word in the string
    let fword (extrf: string) (i : int) =
        // matches in the list are put in importance hierarcy, there is no overlap so it doesn't actually matter currently
        let x = op.Match(extrf, i)
        List.filter (fun x ->
            let (y : Match) = first2 x in y.Success)
                (
                    (separator.Match(extrf, i), SEP) ::
                    (x, OP(match x.Value with | "*" -> MUL | "/" -> DIV | "+" -> ADD | "-" -> SUB | _ -> MUL)) ::
                    (num.Match(extrf, i), NUM) :: []
                )

    let tokenize (data : string * int) (add : int -> int) =
        let string = first2 data
        let index = second2 data
        if index >= string.Length
        then Success((string, index, Token(EOF, "")))
        else
            let nextWord = fword string index
            match nextWord with
            | _ when nextWord.Length > 1 || nextWord.Length <= 0 -> Error($"Error matching at {index}")
            | _ -> let word = first2 nextWord.Head in Success((string, index + (add word.Length), Token(second2 nextWord.Head, word.Value)))

    let nextToken (data : string * int) =
        tokenize data (fun (x : int) -> x)
     
    let peekToken (data : string * int) =
        tokenize data (fun (x : int) -> 0)