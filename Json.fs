// Copyright (C) 2014 Eric Butler (zantifon@gmail.com)

/// <summary>
/// A lightweight JSON parser/formatter and related tools for processing JSON data.
/// </summary>
/// <remarks>
/// This module's goal is a lightweight JSON lib with a F# discriminated untion AST representation that doesn't have external dependencies (other than F# 2.0).
/// This is only a partial implementation of the JSON spec: e.g., it supports integers rather than numbers, some escape codes may be unsupported.
/// It also aims to allow for simple parsing of data structures from JSON ast (for use with storing data files) without using schemas.
/// The json processing functions that come with the AST throw exceptions which can be paired with meta data to allow tools to pinpoint problematic data, without a significant burden on the external code that processes JSON.
/// </remarks>
module TinyJson.Json

open System.Collections
open System.Collections.Generic
open System.Text

// TYPE DEFINITIONS, (and awkwardly the serialization code because ToString needs to be overriden right the frick NOW)
////////////////////////////////////////////////////////////////////////////////

/// the Json AST.
type JsonValue =
| Null
| Int of int
| String of string
| Bool of bool
| Array of JsonArray
| Object of JsonObject
with
    /// Format json with little wasted space to a TextWriter.
    member t.SerializeTo (writer:System.IO.TextWriter) =
        match t with
        | Null -> writer.Write "null"
        | Int i -> writer.Write i
        | String s ->
            let s = s.Replace("\\", "\\\\").Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r").Replace("\"", "\\\"")
            writer.Write '\"'
            writer.Write s
            writer.Write '\"'
        | Bool b -> writer.Write (if b then "true" else "false")
        | Array (ArrayValue a) ->
            writer.Write '['
            a |> Array.iteri (fun i x ->
                if i > 0 then writer.Write ", "
                x.SerializeTo writer
            )
            writer.Write ']'
        | Object (ObjectValue o) ->
            writer.Write '{'
            o |> Array.iteri (fun i (k,v) ->
                if i > 0 then writer.Write ", "
                writer.Write '"'
                writer.Write k
                writer.Write '"'
                writer.Write ':'
                v.SerializeTo writer
            )
            writer.Write '}'

    /// Format json with little wasted space. There's still spaces after ',' though.
    /// Keys will be in alphabetical order, meaning equivalent JSON values are guaranteed to serialize to equal strings.
    member t.Serialize () =
        let sb = StringBuilder ()
        use sw = new System.IO.StringWriter(sb)
        t.SerializeTo sw
        sb.ToString ()

    override t.ToString () = t.Serialize ()
and JsonArray = private ArrayValue of JsonValue[]
and JsonObject = private ObjectValue of (string * JsonValue)[]

/// Some of the error codes used by <c>SyntaxException</c>.
type SyntaxErrorCode =
| InternalError = 1001
| UnexpectedEOF = 1002
| ExpectedCharacter = 1003
| InvalidEscapeCharacter = 1004
| MultilineString = 1005
| InvalidKeyword = 1006
| InvalidCharacter = 1007
| TrailingCharacters = 1008
| DuplicateFieldName = 1009

/// Some of the error codes used by <c>JsonException</c>.
type JsonErrorCode =
| TypeMismatch = 9002
| KeyNotFound = 9003

// Thrown when parsing fails.
[<Sealed>]
type SyntaxException (code: int, location: TextLocation, errorMessage: string, inner: System.Exception) =
    inherit System.Exception(sprintf "%O: Error %d: %s" location code errorMessage, inner)
    /// The error code.
    member x.Code = code
    /// The location in the text where the error occurred.
    member x.Location = location
    /// The detailed message about the error. For a string message including other fields in one string, use this.Message.
    member x.ErrorMessage = errorMessage

/// Base class for any errors produced by the helper functions.
type JsonException (code: int, value: JsonValue, message: string, inner: System.Exception) =
    inherit System.Exception (message, inner)
    /// The json value that this exception references.
    member this.Value = value

/// <summary>Thrown when a requested type does not match (e.g., <c>(String "foo").AsBool</c>).</summary>
[<Sealed>]
type TypeMismatchException (value:JsonValue, typ:string, message:string, inner:System.Exception) =
    inherit JsonException (int JsonErrorCode.TypeMismatch, value, sprintf "Value was not of expected type '%s': %s" typ message, inner)
    /// The type that was expected.
    member this.Expected = typ

/// Thrown when a field is not present in an object.
[<Sealed>]
type KeyNotFoundException (obj:JsonValue, key:string, message:string, inner:System.Exception) =
    inherit JsonException (int JsonErrorCode.KeyNotFound, obj, sprintf "Object does not contain field '%s': %s" key message, inner)
    /// The field name that was expected.
    member this.Key = key

let private typeError v t = raise (TypeMismatchException (v, t, "", null))

// CONSTRUCTION/EXTRACTION FUNCTIONS
////////////////////////////////////////////////////////////////////////////////

module JArray =
    let ofArray x = ArrayValue (Array.copy x)
    let ofList x = ArrayValue (Array.ofList x)
    let ofSeq x = ArrayValue (Array.ofSeq x)

    let toArray (ArrayValue x) = Array.copy x
    let toList (ArrayValue x) = Array.toList x
    let toSeq (ArrayValue x) = Array.toSeq x

module JObject =
    let ofArray x = Array.sortBy fst x |> ObjectValue
    let ofList x = Array.ofList x |> Array.sortBy fst |> ObjectValue
    let ofMap x = ObjectValue (Map.toArray x)
    let ofSeq x = Array.ofSeq x |> Array.sortBy fst |> ObjectValue
    let ofDict (x:KeyValuePair<string, JsonValue> seq) = x |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |> ofSeq

    let toSeq (ObjectValue x) = Array.toSeq x
    let toMap (ObjectValue x) = Map.ofArray x

/// Construct a json object from an array of name, value pairs.
let objectOfArray seq = Object (JObject.ofArray seq)
/// Construct a json object from a generic <c>IDictionary</c>.
let objectOfDict seq = Object (JObject.ofDict seq)
/// Construct a json object from a list of name, value pairs.
let objectOfList seq = Object (JObject.ofList seq)
/// Construct a json object from a <c>Map</c>.
let objectOfMap seq = Object (JObject.ofMap seq)
/// Construct a json object from a sequence of name, value pairs.
let objectOfSeq seq = Object (JObject.ofSeq seq)

let emptyObject = Object (ObjectValue [||])

/// Construct a json array from a <c>JsonValue[]</c>.
let arrayOfArray seq = Array (JArray.ofArray seq)
/// Construct a json array from a <c>list</c>.
let arrayOfList seq = Array (JArray.ofList seq)
/// Construct a json array from a <c>seq</c>.
let arrayOfSeq seq = Array (JArray.ofSeq seq)

let emptyArray = Array (ArrayValue [||])

/// <summary>Cast this value to an int.</summary>
/// <exception cref="TypeMismatchException">If this value is not an int.</exception>
let asInt j = match j with Int(x) -> x | _ -> typeError j "int"
/// <summary>Cast this value to an string.</summary>
/// <exception cref="TypeMismatchException">If this value is not an string.</exception>
let asString j = match j with String(x) -> x | _ -> typeError j "string"
/// <summary>Cast this value to an bool.</summary>
/// <exception cref="TypeMismatchException">If this value is not an bool.</exception>
let asBool j = match j with Bool(x) -> x | _ -> typeError j "bool"
/// <summary>Cast this value to an json array.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json array.</exception>
let asArray j = match j with Array(x) -> x | _ -> typeError j "array"
/// <summary>Cast this value to an json object.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json object.</exception>
let asObject j = match j with Object(x) -> x | _ -> typeError j "object"
/// <summary>Try to find the given field of the given object.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json object.</exception>
let tryGetField j f =
    let (ObjectValue arr) = asObject j
    arr |> Array.tryPick (fun (k,v) -> if k = f then Some v else None)
/// <summary>Return the given field of the given object.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json object.</exception>
/// <exception cref="KeyNotFoundException">If this object does not have the given field.</exception>
let getField j f = match tryGetField j f with Some v -> v | None -> raise (KeyNotFoundException (j, f, "", null))

let setField j (key, value) =
    match asObject j with
        ObjectValue o ->
            let newO =
                match Array.tryFindIndex (fun (k,_) -> k = key) o with
                | None -> Array.append o [| key, value |]
                | Some i ->
                    let a = Array.copy o
                    a.[i] <- (key, value)
                    a
            Object (ObjectValue newO)

/// <summary>Cast this value to an json object and convert to a <c>Map</c>.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json object.</exception>
let objectToMap j = asObject j |> JObject.toMap
/// <summary>Cast this value to an json object and convert to a <c>seq</c>.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json object.</exception>
let objectToSeq j = asObject j |> JObject.toSeq

/// <summary>Cast this value to an json array and convert to a <c>JsonValue[]</c>.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json array.</exception>
let arrayToArray j = asArray j |> JArray.toArray
/// <summary>Cast this value to an json array and convert to a <c>JsonValue list</c>.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json array.</exception>
let arrayToList j = asArray j |> JArray.toList
/// <summary>Cast this value to an json array and convert to a <c>JsonValue seq</c>.</summary>
/// <exception cref="TypeMismatchException">If this value is not an json array.</exception>
let arrayToSeq j = asArray j |> JArray.toSeq

// ALL THE ABOVE FUNCTIONS, BUT AS CLASS MEMBERS
////////////////////////////////////////////////////////////////////////////////

type JsonValue with
    member t.AsInt = asInt t
    member t.AsString = asString t
    member t.AsBool = asBool t
    member t.AsArray = asArray t
    member t.AsObject = asObject t
    member t.TryGetField key = tryGetField t key
    member t.GetField key = getField t key

    member t.Load key (fn:System.Func<JsonValue,'a>) = fn.Invoke (getField t key)
    member t.TryLoadOrElse key (fn:System.Func<JsonValue,'a>) default_:'a =
        match tryGetField t key with None -> default_ | Some x -> fn.Invoke(x)

    member t.SetField (key, value) = setField t (key, value)

    static member EmptyObject = emptyObject
    static member ObjectOf seq = objectOfArray seq
    static member ObjectOf seq = objectOfList seq
    static member ObjectOf seq = objectOfDict seq
    static member ObjectOf seq = objectOfMap seq
    static member ObjectOf seq = objectOfSeq seq

    static member EmptyArray = emptyArray
    static member ArrayOf seq = arrayOfArray seq
    static member ArrayOf seq = arrayOfList seq
    static member ArrayOf seq = arrayOfSeq seq

type JsonArray with
    member t.ToArray = JArray.toArray t
    member t.ToList = JArray.toList t
    member t.ToSeq = JArray.toSeq t

type JsonObject with
    member t.ToMap = JObject.toMap t
    member t.ToSeq = JObject.toSeq t

/// active pattern that checks if an object implements generic IDicationay<'a,'b> for any type and if so returns it as an IEnumerable of key-value tuples
let private (|DictType|_|) (obj:obj) : seq<obj * obj> option =
    let interfaces = obj.GetType().GetInterfaces()
    let dictType = typeof<IDictionary<_,_>>.GetGenericTypeDefinition()
    if interfaces |> Array.exists (fun t -> t.IsGenericType && t.GetGenericTypeDefinition() = dictType)
    then
        let arr = obj :?> IEnumerable |> Seq.cast |> Seq.cache
        if Seq.isEmpty arr
        then Some Seq.empty
        else
            let kvpType = (Seq.head arr).GetType()
            let keyProp = kvpType.GetProperty("Key")
            let valProp = kvpType.GetProperty("Value")
            Some (arr |> Seq.map (fun o -> (keyProp.GetValue (o, null), valProp.GetValue (o, null))))
    else None

/// <summary>Create a <c>JsonValue</c> from any <c>System.Object</c>.
/// Will interpret any <c>IDictionary</c> (generic or not) as an object, any other <c>IEnumerable</c> as an array, enum types as strings, and primitive types appropriately.
/// <c>Guid</c>s are output as strings.
/// Any other objects will cause an error.</summary>
/// <remarks>Obviously pretty slow because it needs to use a lot of casting and reflection to work (especially for json objects).
/// If performance is important, prefer the direct construction functions.</remarks>
let rec fromObject (obj:obj) =
    match obj with
    | null -> Null
    | :? string as x -> String x
    | :? int as x -> Int x
    | :? bool as x -> Bool x
    | :? System.Guid as g -> String (g.ToString ())
    | DictType(seq) -> seq |> Seq.map (fun (k,v) -> (k :?> string, fromObject v)) |> objectOfSeq
    | :? IDictionary as x -> Seq.cast<DictionaryEntry> x |> Seq.map (fun kvp -> (kvp.Key :?> string, fromObject kvp.Value)) |> objectOfSeq
    | :? IEnumerable as x -> Seq.cast x |> Seq.map fromObject |> arrayOfSeq
    | _ when obj.GetType().IsEnum -> String (System.Enum.GetName(obj.GetType(), obj))
    | _ -> invalidArg "obj" (sprintf "Cannot convert type '%s' to json!" (obj.GetType().Name))

/// Information about a JsonValue's relation to it's parent.
[<RequireQualifiedAccess>]
type ChildType =
/// Value is a field of an object with this field name.
| Field of string
/// Value is an entry of an array with this index.
| Index of int
/// Value has no parent; it is the root.
| Root

/// Meta data associated with a JSON ast node.
/// We wish to have meta data about the JSON parse tree to have machine-identifiable errors with JSON data.
/// Meta data is represented as a separated object (accessible via hashmap) in order to simplify the AST, desireable for two reasons:
/// 1) Many consumers simply won't care about the metadata so we don't want it complicating usage.
/// 2) Program-generated json is very common, and having to fabricate this data (or even fill in empty data) would complicate usage.
/// The map uses reference equality, so, obviously, if an AST node is replaced with an equivalent one, it will no longer work.
type Meta = {
    /// The location of this value in the concrete text.
    Location: TextLocation;
    /// How this value is referenced from its immediate ancestor.
    /// Field(str) means it's the "str" field on a JsonObject,
    /// Index(n) means it's the nth entry of a JsonArray,
    /// while Root means it's at the root of the hierarchy.
    ChildType: ChildType;
    /// The parent of this json object, will be either an array or object (none if root).
    Parent: JsonValue option;
}

type ParseResult = {
    Ast: JsonValue;
    // we have to use a mutable dictionary since Map doesn't let you override equality and we need reference equality >_> (and we're stuck with .NET 3.5... and I'm too lazy to find/write an immutable map)
    Meta: Dictionary<JsonValue, Meta>;
}

type private Parser (program, filename, doParseMeta) =
    let cs = TextStream (program, filename, fun loc -> upcast SyntaxException (int SyntaxErrorCode.UnexpectedEOF, loc, "Unexpected EOF.", null))
    let meta = if doParseMeta then Dictionary<JsonValue, Meta> Collections.HashIdentity.Reference else null

    let syntaxError (code:SyntaxErrorCode) (pstart,pend) message = raise (SyntaxException (int code, TextLocation (pstart, pend, cs.Filename), message, null))

    let isSpace c = c = ' ' || c = '\r' || c = '\n' || c = '\t'

    let isNewline c = c = '\n'

    let skipWhitespace () = while (not cs.IsEOF) && (isSpace cs.Peek) do ignore (cs.Next ())

    let takeWhile (c:char) f =
        let b = StringBuilder ()
        b.Append c |> ignore
        while (not cs.IsEOF) && f cs.Peek do b.Append (cs.Next ()) |> ignore
        b.ToString ()

    let matchCharacter c =
        let p = cs.Position
        let n = cs.Next()
        if (n <> c) then syntaxError SyntaxErrorCode.ExpectedCharacter (p,cs.Position) (sprintf "expected character %O, got %O" c n)
        skipWhitespace ()

    let parseString doReadOpeningQuote =
        if doReadOpeningQuote then matchCharacter '"' 
        let start = cs.Position // not quite correct pos, but MEH
        let b = StringBuilder ()
        while not (cs.Peek = '"' || isNewline cs.Peek) do
            let c = cs.Next()
            if c = '\\'
            then
                let ec =
                    match cs.Next() with
                    | 'n' -> '\n'
                    | 't' -> '\t'
                    | 'r' -> '\r'
                    | '"' -> '"'
                    | '\\' -> '\\'
                    | x -> syntaxError SyntaxErrorCode.InvalidEscapeCharacter (cs.Position,cs.Position) (sprintf "invalid escape character '\%c'" x)
                b.Append ec |> ignore
            else b.Append c |> ignore
        let s = b.ToString ()
        // if there is a newline, that's an error, otherwise skip the right quote
        let p = cs.Position
        if isNewline (cs.Next()) then syntaxError SyntaxErrorCode.MultilineString (start,p) "String literals may not span multiple lines"
        skipWhitespace ()
        s

    // recursively parse the json value, generating the meta data as we go
    let rec parse (childType: ChildType) =
        let start = cs.Position

        skipWhitespace ()

        let value, children =
            match cs.Next () with
            | '{' ->
                skipWhitespace ()
                let items = List()
                let fields = HashSet()
                while not (cs.Peek = '}') do
                    let kstart = cs.Position
                    let key = parseString true
                    if not (fields.Add key) then syntaxError SyntaxErrorCode.DuplicateFieldName (kstart, cs.Position) (sprintf "Duplicate field '%s' in object" key)
                    matchCharacter ':'
                    let value = parse (ChildType.Field key)
                    items.Add ((key, value)) |> ignore
                    if not (cs.Peek = '}') then matchCharacter ','
                cs.Next () |> ignore // skip final }
                let itemArray = items.ToArray ()
                // keep fields in sorted order so structural equality behaves nicely
                Array.sortInPlaceBy fst itemArray
                Object (ObjectValue itemArray), Seq.map snd items
            | '[' ->
                let values = List()
                let mutable counter = 0
                while not (cs.Peek = ']') do
                    values.Add (parse (ChildType.Index counter)) |> ignore
                    counter <- counter + 1
                    if not (cs.Peek = ']') then matchCharacter ','
                cs.Next () |> ignore // skip final ]
                Array (ArrayValue (values.ToArray ())), upcast values
            | c when System.Char.IsDigit c ->
                Int (System.Int32.Parse (takeWhile c System.Char.IsDigit)), Seq.empty
            | c when System.Char.IsLetter c ->
                let value =
                    match takeWhile c System.Char.IsLetter with
                    | "true" -> Bool true
                    | "false" -> Bool false
                    | "null" -> Null
                    | _ -> syntaxError SyntaxErrorCode.InvalidKeyword (start,cs.Position) "invalid keyword"
                value, Seq.empty
            | '"' -> String (parseString false), Seq.empty
            | c -> syntaxError SyntaxErrorCode.InvalidCharacter (start,start) (sprintf "invalid character %O" c)

        skipWhitespace ()

        let lend = cs.Position
        if doParseMeta then
            // generate meta (but with no parent; we don't know the parent yet since it hasn't been parsed!)
            meta.Add (value, {Location=TextLocation(start, lend, cs.Filename); ChildType=childType; Parent=None;})
            // update all children's meta now that we have the parent object
            for child in children do
                meta.[child] <- {meta.[child] with Parent=Some value}

        value

    member x.Parse () =
        try
            let ast = parse ChildType.Root
            if not cs.IsEOF then syntaxError SyntaxErrorCode.TrailingCharacters (cs.Position, cs.Position) ("Unepxcted trailing characters")
            {Ast=ast; Meta=meta}
        with
        | :? SyntaxException -> reraise ()
        | e -> raise (SyntaxException (int SyntaxErrorCode.InternalError, TextLocation.Empty, "Internal parser error.", e))

/// <summary>Parse a string into a <c>JsonValue</c> returning both the parsed json and meta information about the parse tree.
/// This information can be used to, for example, map json values back to locations in the original text.</summary>
/// <exception cref="SyntaxException">If the string is malformed.</exception>
let ParseWithMeta string filename = Parser(string, filename, true).Parse()

/// <summary>Parse a string into a <c>JsonValue</c></summary>
/// <exception cref="SyntaxException">If the string is malformed.</exception>
let Parse string = Parser(string, "", false).Parse().Ast

let SerializeTo (writer:System.IO.TextWriter) (value:JsonValue) = value.SerializeTo writer
let Serialize (value:JsonValue) = value.Serialize ()

/// Formats json in the following way:
/// the top level object has each field on a new line,
/// sub objects up to maxIndent levels deep are pretty-printed with 4-space indent, everything on new line,
/// past maxIndent levels, everything else is smashed together without newlines.
let PrettyPrint maxIndent json =
    let rec formatVal indent jval =
        let indentStr = new System.String(' ', 4 * indent)
        let indentStr2 = new System.String(' ', 4 * (max 0 (indent - 1)))
        match jval with
        | Object (ObjectValue o) when indent < maxIndent && o.Length > 0 ->
            let x = o |> Array.map (fun (k,v) -> sprintf "%s\"%s\": %s" indentStr k (formatVal (indent + 1) v))
            sprintf "{\n%s\n%s}" (System.String.Join (",\n", x)) indentStr2
        | Array (ArrayValue a) when indent < maxIndent && a.Length > 0 ->
            let x = a |> Array.map (fun v -> sprintf "%s%s" indentStr (formatVal (indent + 1) v))
            sprintf "[\n%s\n%s]" (System.String.Join (",\n", x)) indentStr2
        | _ ->
            Serialize jval

    formatVal 0 json
