module ParserDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

let run () =
    let dummyFile = "C:\\test.fsx"

    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let getUntypedTree (file, input) = 
      let projectOptions, _errors =  
          checker.GetProjectOptionsFromScript(file, input)
          |> Async.RunSynchronously

      let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projectOptions)

      let parseFileResults = 
          checker.ParseFile(file, input, parsingOptions) 
          |> Async.RunSynchronously

      match parseFileResults.ParseTree with
      | Some tree -> tree
      | None -> failwith "Something went wrong during parsing!"

    let code = "let tuple : int * int = 42, 42"

    let tree = getUntypedTree(dummyFile, code)

    match tree with
    | ParsedInput.ImplFile(implFile) ->
        let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
        printfn "%A" modules
        modules
    | _ -> failwith "F# Interface file (*.fsi) not supported."