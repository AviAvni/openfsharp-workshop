module TypeCheckerDemo

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

module Inputs = 
    let base1 = Path.GetTempFileName()
    let fileName1 = Path.ChangeExtension(base1, ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")

let run () =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let code = "let tuple : int * int = 42, 42"

    let parseAndCheckSingleFile input = 
        File.WriteAllText(Inputs.fileName1, input)
        let projectOptions = 
            let sdkLib nm = 
                let sysDir = @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.1.0\ref\netcoreapp2.1"
                let (++) a b = System.IO.Path.Combine(a,b)
                sysDir ++ nm + ".dll"                 

            checker.GetProjectOptionsFromCommandLineArgs
               (Inputs.projFileName,
                [| yield "--simpleresolution" 
                   yield "--noframework" 
                   yield "--debug:full" 
                   yield "--define:DEBUG" 
                   yield "--optimize-" 
                   yield "--out:" + Inputs.dllName
                   yield "--doc:test.xml" 
                   yield "--warn:3" 
                   yield "--fullpaths" 
                   yield "--flaterrors" 
                   yield "--target:exe" 
                   yield Inputs.fileName1
                   let references =
                     [ sdkLib "mscorlib" 
                       sdkLib "System"
                       sdkLib "System.Core"
                       sdkLib "System.IO"
                       sdkLib "System.Runtime"
                       @"C:\Users\avi\.nuget\packages\fsharp.core\4.5.2\lib\netstandard1.6\FSharp.Core.dll" ]
                   for r in references do 
                         yield "-r:" + r |])

        checker.ParseAndCheckProject(projectOptions) 
        |> Async.RunSynchronously

    let parseFileResults = parseAndCheckSingleFile code

    printfn "%A" parseFileResults.AssemblyContents.ImplementationFiles.[0].Declarations