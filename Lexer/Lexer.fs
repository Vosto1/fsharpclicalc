namespace Calculator.Lexer

open Calculator.Core.Tuple
open Calculator.Core.AbstractSyntax
open Calculator.Core.Result
module Lexer = 
    open System.Text.RegularExpressions
    type TokenType = NUM | OP of Operator | SEP of Separator | EOF | FUNC of FunctionType | CONST of ConstantType
    type Token = TokenType * string
    let (separator : Regex) = new Regex("\G(\(|\)|\,)")
    let (op : Regex) = new Regex("\G(\*\*|\*|\+|\/|\-|\^)") // pattern order matters!!
    let (num : Regex) = new Regex("\G([0-9]+(\.[0-9]+)?)")
    let (builtinFunctions : Regex) = new Regex("\G(root)")
    let (builtinConstants : Regex) = new Regex("\G(e|pi)")
    
    // get next word in the string
    // TODO: add whitespace handler
    let fword (extrf: string) (i : int) =
        // matches in the list are put in importance hierarcy, there is no overlap so it doesn't actually matter currently
        let operator = op.Match(extrf, i)
        let fn = builtinFunctions.Match(extrf, i)
        let sep = separator.Match(extrf, i)
        let constant = builtinConstants.Match(extrf, i)
        List.filter (fun x ->
            let (y : Match) = first2 x in y.Success)
                (
                    (sep, SEP(match sep.Value with | "(" -> LPAR | ")" -> RPAR | "," -> COMMA | _ -> LPAR)) ::
                    (operator, OP(match operator.Value with | "*" -> MUL | "/" -> DIV | "+" -> ADD | "-" -> SUB | "^" -> POWER | "**" -> POWER | _ -> MUL)) ::
                    (num.Match(extrf, i), NUM) :: 
                    (fn, FUNC(match fn.Value with | "root" -> ROOT | _ -> ROOT)) ::
                    (constant, CONST(match constant.Value with | "e" -> EULER | "pi" -> PI | _ -> EULER)) :: []
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