module CodeGenerationDemo

open System.Text
open System.IO
open Microsoft.FSharp.Compiler.Interactive.Shell

let run () =
    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    // Build command line arguments & start FSI session
    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)

    let evalExpression text =
      match fsiSession.EvalExpression(text) with
      | Some value -> printfn "%A" value.ReflectionValue
      | None -> printfn "Got no result!"

    evalExpression "42 + 1"