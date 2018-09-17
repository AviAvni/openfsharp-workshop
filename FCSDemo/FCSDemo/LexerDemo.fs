module LexerDemo

open Microsoft.FSharp.Compiler.SourceCodeServices

let run () =
    let dummyFile = "C:\\test.fsx"

    let sourceTok = FSharpSourceTokenizer([], Some dummyFile)

    let code = "let answer = 42"

    let tokenizer = sourceTok.CreateLineTokenizer(code)

    let rec tokenizeLine (tokenizer:FSharpLineTokenizer) state =
      match tokenizer.ScanToken(state) with
      | Some tok, state ->
          printfn "%s %O" tok.TokenName tok
          tokenizeLine tokenizer state
      | None, state -> state

    tokenizeLine tokenizer 0L