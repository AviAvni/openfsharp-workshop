open System
open System.IO

Console.ReadLine ()
printfn "---------------------"
printfn "|     Tokenizer     |"
printfn "---------------------"
LexerDemo.run ()

Console.ReadLine ()
printfn "----------------------"
printfn "|       Parser       |"
printfn "----------------------"

let modules = ParserDemo.run ()
Visualize.visualize modules

Console.ReadLine ()
printfn "----------------------"
printfn "|    Type Checker    |"
printfn "----------------------"

TypeCheckerDemo.run ()

Console.ReadLine ()
printfn "----------------------"
printfn "|   Code Generator   |"
printfn "----------------------"

CodeGenerationDemo.run ()