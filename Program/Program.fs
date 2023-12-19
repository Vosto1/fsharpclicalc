namespace Calculator.Program

open Calculator.Interpreter.Interpreter
open Calculator.Core.Tuple
open Calculator.Parser.Parser
open Calculator.Lexer.Lexer
module Program =

    let rec LexAndPrint string index  =
        let tok = nextToken (string, index) in
        let t = last3 tok in 
        match first2 t with
        | EOF -> ()
        | _ -> printfn "%s %s %d" (second2 t) ((first2 t).ToString()) (middle3 tok)|> ignore; LexAndPrint (first3 tok) (middle3 tok) 

    let rec run () =
        printf ">"
        let program = System.Console.ReadLine ()
        match program with
        | "quit" -> ()
        | _ -> 
            System.Console.Clear ()
            printfn "write an expression.\n\"quit\" to stop the program."
            printfn "------------------------------------------------------------------------------------"
            printfn ">%s\n" program
            try
                LexAndPrint program 0 |> ignore
                let expr = (parse program)
                match expr with
                | (success, expression) when success -> 
                    printfn ""
                    printfn "Parse successful\n%A" expression
                    printfn "\nResult: %f" (second2 (Evaluate expression))
                | _ -> printfn "Parse failed."
            with
                | MatchError(x) -> printfn "Lexing failed: %s" x
            printfn "------------------------------------------------------------------------------------"
            run ()

    [<EntryPoint>]
    let Main (args : string[]) : int =
        printfn "write an expression.\n\"quit\" to stop the program."
        run ()
        0