// Copyright (C) 2014 Eric Butler (zantifon@gmail.com)

/// <summary>
/// Basic types used for parsing and serialization.
/// </summary>
namespace TinyJson

/// A position in a text document (e.g., source code). Contains line and column, both zero-indexed.
[<Struct;StructuralEquality;NoComparison>]
type TextPosition (l:int, c:int) =
    member x.Line = l
    member x.Col = c
    override x.ToString () = if x.Line >= 0 then sprintf "(%d,%d)" (l+1) (c+1) else "*"

    static member Empty = TextPosition (-1,-1)

/// Represents a ranged location in a text document between two TextPositions.
/// Includes the start and end (both inclusive) as well as the filename.
[<Struct;StructuralEquality;NoComparison>]
type TextLocation (s: TextPosition, e: TextPosition, f: string) =
    member x.Start = s
    member x.End = e
    member x.Filename = f
    override x.ToString () = sprintf "%s%O-%O" f s e

    static member Empty = TextLocation (TextPosition.Empty, TextPosition.Empty, "")
    static member Unknown filename = TextLocation (TextPosition.Empty, TextPosition.Empty, filename)

/// <summary>Takes an input text as a string and exposes it as a stream, computing TextPosition as it goes.</summary>
/// <param name="stream">The text to expose as a stream.</param>
/// <param name="filename">A resource name for the source text. Exposed in the member Filename</param>
/// <param name="eoferrorfn">A function that will be invoked by TextStream when an EOF occurs, i.e., reading past the end of the source text.
/// TextStream will raise the exception returned by this function.</param>
[<Sealed>]
type TextStream (stream: string, filename: string, eoferrorfn: TextLocation -> exn) =
    let mutable index = 0
    let mutable line = 0
    let mutable col = 0

    let eoferror () =
        let p = TextPosition (line, col)
        let e = eoferrorfn (TextLocation (p, p, filename))
        raise e

    let get idx =
        try
          stream.[idx]
        with :? System.IndexOutOfRangeException -> eoferror ()

    member x.Next () =
        // grab char and position
        let c = get index

        // move reader head forward
        index <- index + 1
        // update position
        if c = '\n' then
            line <- line + 1
            col <- 0
        else if not (System.Char.IsControl c) then
            col <- col + 1

        c

    member x.Peek = get index
    member x.PeekAt d = get (index + d)
    member x.TryPeekAt d = if index + d < stream.Length then Some stream.[index + d] else None
    member x.IsEOF = stream.Length <= index
    member x.Filename = filename
    /// the position of the NEXT character to be lexed
    member x.Position = TextPosition (line, col)
