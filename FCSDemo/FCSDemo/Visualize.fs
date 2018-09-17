module Visualize

open System.Diagnostics
open SixLabors.ImageSharp
open SixLabors.Primitives
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open TypeShape.Core.Utils
open SixLabors.Fonts
open TypeShape.Core
open Microsoft.FSharp.Compiler.Range
open System
open Microsoft.FSharp.Compiler.Ast
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Compiler.SourceCodeServices

let openFile name =
    let info = new ProcessStartInfo(name)
    info.UseShellExecute <- true
    Process.Start(info) |> ignore

let width = 8000
let height = 2000
let img = new Image<Rgba32>(width, height)

type Input<'T> = PointF * PointF * PointF * PointF * int * 'T

let center (p1:PointF, p2:PointF) =
    let res = PointF((p1.X + p2.X) / 2.0f, p1.Y)
    p1

let distribute (i:int) (c:int) (ident) (p1:PointF, p2:PointF) (pp1:PointF, pp2:PointF) d u =
    if c = 1 && not ident then p1, p2, pp1, pp2, d, u else

    let l = p2 - p1
    let o = 40.0f
    if l.X >= l.Y then
        let o = if l.X >= o then o else 0.0f
        let sl = PointF(o + l.X / float32 c * float32 i, o)
        let el = PointF(l.X / float32 c * float32 (c - i - 1), 0.0f)
        p1 + sl, p2 - el, p1, p2, d + 1, u
    else
        let o = if l.Y >= o then o else 0.0f
        let sl = PointF(o, o + l.Y / float32 c * float32 i)
        let el = PointF(0.0f, l.Y / float32 c * float32 (c - i - 1))
        p1 + sl, p2 - el, p1, p2, d + 1, u

let rec mkPrinter<'T> (imgCtx:IImageProcessingContext<Rgba32>) : Input<'T> -> unit =
    let ctx = new TypeGenerationContext()
    mkPrinterCached<'T> ctx imgCtx

and mkPrinterCached<'T> (ctx : TypeGenerationContext) (imgCtx:IImageProcessingContext<Rgba32>) : Input<'T> -> unit =
    match ctx.InitOrGetCachedValue<Input<'T> -> unit> (fun c t -> c.Value t) with
    | Cached(value = p) -> p
    | NotCached t ->
        let p = mkPrinterAux<'T> ctx imgCtx
        ctx.Commit t p

and mkPrinterAux<'T> (ctx : TypeGenerationContext) (imgCtx:IImageProcessingContext<Rgba32>) : Input<'T> -> unit =
    let wrap(p : Input<'a> -> unit) = unbox<Input<'T> -> unit> p
    let drawText txt (p1, p2:PointF) (pp1, pp2) = 
        let font = SystemFonts.CreateFont("Arial", 20.0f, FontStyle.Regular)
        let pen = Pens.Solid(Rgba32.Black, 1.0f)
        imgCtx.DrawLines(pen, [|center (p1, p2); center (pp1, pp2)|]).DrawText(txt, font, Rgba32.Black, center (p1, p2)) |> ignore
    let mkFieldPrinter (field : IShapeMember<'DeclaringType>) =
        field.Accept {
            new IMemberVisitor<'DeclaringType, string * (Input<'DeclaringType> -> unit)> with
                member __.Visit(field : ShapeMember<'DeclaringType, 'Field>) =
                    let fp = mkPrinterCached<'Field> ctx imgCtx
                    field.Label, fun (p1, p2, pp1, pp2, d, t) -> fp (p1, p2, pp1, pp2, d + 1, field.Project t)
        }

    match shapeof<'T> with
    | Shape.Guid -> wrap(fun (p1, p2, pp1, pp2, d, s:Guid) -> ())
    | Shape.String -> wrap(fun (p1, p2, pp1, pp2, d, s:string) -> ())
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<Input<'T> -> unit> with
                member __.Visit<'a> () = // 'T = 'a option
                    let tp = mkPrinterCached<'a> ctx imgCtx
                    wrap(fun (p1, p2, pp1, pp2, d, (o : 'a option)) -> match o with None -> () | Some t -> tp (p1, p2, pp1, pp2, d, t))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<Input<'T> -> unit> with
                member __.Visit<'a> () = // 'T = 'a list
                    let tp = mkPrinterCached<'a> ctx imgCtx
                    wrap(fun (p1, p2, pp1, pp2, d, (ts : 'a list)) -> (if ts.Length > 1 then drawText "List" (p1, p2) (pp1, pp2)); ts |> Seq.iteri (fun i t -> tp (distribute i (ts.Length) false (p1, p2) (pp1, pp2) d t)))
        }

    //| Shape.Array s when s.Rank = 1 ->
    //    s.Accept {
    //        new IArrayVisitor<'T -> string> with
    //            member __.Visit<'a> _ = // 'T = 'a []
    //                let tp = mkPrinterCached<'a> ctx
    //                wrap(fun (ts : 'a []) -> ts |> Seq.map tp |> String.concat "; " |> sprintf "[|%s|]")
    //    }

    //| Shape.FSharpSet s ->
    //    s.Accept {
    //        new IFSharpSetVisitor<'T -> string> with
    //            member __.Visit<'a when 'a : comparison> () =  // 'T = Set<'a>
    //                let tp = mkPrinterCached<'a> ctx
    //                wrap(fun (s : Set<'a>) -> s |> Seq.map tp |> String.concat "; " |> sprintf "set [%s]")
    //    }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemPrinters = shape.Elements |> Array.filter (fun s -> FSharpType.IsUnion (s.MemberInfo :?> PropertyInfo).PropertyType ) |> Array.map mkFieldPrinter
        fun (p1, p2, pp1, pp2, d, (t:'T)) ->
            elemPrinters
            |> Seq.iteri (fun i (label, ep) -> (if elemPrinters.Length > 1 then drawText "Tuple" (p1, p2) (pp1, pp2)); ep (distribute i (elemPrinters.Length) false (p1, p2) (pp1, pp2) d t))

    //| Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
    //    let fieldPrinters = shape.Fields |> Array.map mkFieldPrinter
    //    fun (r:'T) ->
    //        fieldPrinters
    //        |> Seq.map (fun (label, ep) -> let value = ep r in sprintf "%s = %s" label value)
    //        |> String.concat "; "
    //        |> sprintf "{ %s }"

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let mkUnionCasePrinter (s : ShapeFSharpUnionCase<'T>) =
            let fieldPrinters = s.Fields |> Array.filter (fun s -> FSharpType.IsUnion (s.MemberInfo :?> PropertyInfo).PropertyType ) |> Array.map mkFieldPrinter
            fun (p1, p2, pp1, pp2, d, (u:'T)) -> 
                drawText (s.CaseInfo.DeclaringType.Name + "." + s.CaseInfo.Name) (p1, p2) (pp1, pp2)
                fieldPrinters |> Seq.iteri (fun i (_,fp) -> fp (distribute i (fieldPrinters.Length) true (p1, p2) (pp1, pp2) d u))

        let casePrinters = shape.UnionCases |> Array.map mkUnionCasePrinter
        fun (p1, p2, pp1, pp2, d, (u:'T)) -> 
            let printer = casePrinters.[shape.GetTag u]
            printer (p1, p2, pp1, pp2, d + 1, u)

    //| Shape.Poco (:? ShapePoco<'T> as shape) ->
    //    if typeof<'T> = typeof<range> then fun (p1, p2, pp1, pp2, d, r) -> drawText "range" (p1, p2) (pp1, pp2) 
    //    elif typeof<'T> = typeof<pos> then fun (p1, p2, pp1, pp2, d, r) -> drawText "pos" (p1, p2) (pp1, pp2) else
    //    let propPrinters = shape.Properties |> Array.filter (fun p -> (p.Label.StartsWith "Start" || p.Label.StartsWith "End") |> not) |> Array.map mkFieldPrinter
    //    fun (p1, p2, pp1, pp2, d, (r:'T)) ->
    //        propPrinters
    //        |> Seq.iteri (fun i (label, ep) -> (if propPrinters.Length > 1 then drawText "Poco" (p1, p2) (pp1, pp2)); ep (distribute i (propPrinters.Length) false (p1, p2) (pp1, pp2) d r))

    | _ -> fun (p1, p2, pp1, pp2, d, r) -> drawText (sprintf "Error %s" typeof<'T>.Name) (p1, p2) (pp1, pp2)

let visualize modules =
    img.Mutate(Action<IImageProcessingContext<_>>(fun ctx -> let p = mkPrinter<SynModuleOrNamespace list> (ctx.Fill(Rgba32.White))
                                                             p (PointF(0.0f, 0.0f), PointF(float32 width, float32 height), PointF.Empty, PointF.Empty, 0, modules)))
    let file = Path.Combine(Environment.CurrentDirectory, "img.png")
    img.Save file
    openFile file