namespace Calculator.Program

open Calculator.Core.Result
open Calculator.Interpreter.Interpreter
open Calculator.Core.Tuple
open Calculator.Parser.Parser
open Calculator.Lexer.Lexer

module Program =

    let rec LexAndPrint string index =
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
            LexAndPrint program 0 |> ignore
            let expr = (parse program)
            Reduce expr (fun success ->
                printfn ""
                printfn "Parse successful\n%A" (first2 success)
                printfn "\nResult: %f" (
                    Reduce (Evaluate (first2 success))
                        (fun result -> result) (fun error -> printf "%s " error; 0.0))
            ) (fun error -> 
                printfn ""
                printfn "Parse failed\n%s" error
            )
            printfn "------------------------------------------------------------------------------------"
            run ()

    [<EntryPoint>]
    let Main (args : string[]) : int =
        printfn "write an expression.\n\"quit\" to stop the program."
        run ()
        0