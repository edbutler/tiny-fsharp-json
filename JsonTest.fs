module JsonTest

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

module J = TinyJson.Json

// basic parsing tests

let tupleSeqToPropertyList seq =
    seq |> Seq.map (fun (x,y) -> [|x :> obj; y :> obj|])

let trivialParseTests = [
    ("[]", J.arrayOfSeq Seq.empty);
    ("{}", J.objectOfSeq Seq.empty);
    ("\"\"", J.String "");
    ("4", J.Int 4);
]
let trivialParseTestsSeq = tupleSeqToPropertyList trivialParseTests

[<Theory>]
[<PropertyData("trivialParseTestsSeq")>]
let ``JSON trivial parse test`` string (jobj: J.JsonValue) =
    J.Parse string |> should equal jobj

let simpleParseTests = [
    ("23", J.Int 23);
    ("4", J.Int 4);
    ("""[2, "pancake", {}, []]""", J.arrayOfSeq [ J.Int 2; J.String "pancake"; J.objectOfSeq Seq.empty; J.arrayOfSeq Seq.empty ]);
    ("""{"a":2, "b":"pancake", "c":{}, "d":[]}""", J.objectOfSeq ["a", J.Int 2; "b", J.String "pancake"; "c", J.objectOfSeq Seq.empty; "d", J.arrayOfSeq Seq.empty]);
    (@"""abc\t\r\n""", J.String "abc\t\r\n");
]
let simpleParseTestsSeq = tupleSeqToPropertyList simpleParseTests

[<Theory>]
[<PropertyData("simpleParseTestsSeq")>]
let ``JSON simple parse test`` string (jobj: J.JsonValue) =
    J.Parse string |> should equal jobj

// basic parsing error tests

[<Fact>]
let ``JSON illegal string test`` () =
    (fun () -> J.Parse "\"asdf" |> ignore) |> should throw typeof<J.SyntaxException>

[<Fact>]
let ``JSON trailing text test`` () =
    (fun () -> J.Parse "2f" |> ignore) |> should throw typeof<J.SyntaxException>

[<Fact>]
let ``JSON illegal object test`` () =
    (fun () -> J.Parse "{whoops:2}" |> ignore) |> should throw typeof<J.SyntaxException>

// formatting

[<Theory>]
[<PropertyData("trivialParseTestsSeq")>]
let ``JSON trivial format test`` string (jobj: J.JsonValue) =
    J.Serialize jobj |> should equal string

[<Theory>]
[<PropertyData("simpleParseTestsSeq")>]
let ``JSON simple format test`` string (jobj: J.JsonValue) =
    J.Serialize jobj |> should equal string

// from object tests

let fromObjectTests = [
    (2 :> obj, J.Int 2);
    ("happy" :> obj, J.String "happy");
    ([|3; 4; 5|] :> obj, J.arrayOfSeq [| J.Int 3; J.Int 4; J.Int 5 |]);
    ([] :> obj, J.arrayOfSeq Seq.empty);
    (["asdf" :> obj; 5 :> obj] :> obj, J.arrayOfSeq [| J.String "asdf"; J.Int 5 |]);
    (Map.empty :> obj, J.objectOfSeq Seq.empty);
    (Map.ofSeq [| "a", 2; "b", 3 |] :> obj, J.objectOfSeq [ "a", J.Int 2; "b", J.Int 3 ]);
]
let fromObjectTestsSeq = tupleSeqToPropertyList fromObjectTests

[<Theory>]
[<PropertyData("fromObjectTestsSeq")>]
let ``JSON fromObject test`` value (jobj: J.JsonValue) =
    J.fromObject value |> should equal jobj


let tripleSeqToPropertyList seq =
    seq |> Seq.map (fun (x,y,z) -> [|x :> obj; y :> obj; z :> obj|])

let equivalenceTests = [
    J.Int 23, J.Int 23, true;
    J.Int 22, J.Int 23, false;
    J.Int 22, J.Null, false;
    J.Null, J.Null, true;
    J.String "asdf", J.String "asdf", true;
    J.String "asdf", J.String "asdfs", false;
    J.arrayOfSeq [J.Int 2], J.arrayOfSeq [J.Int 2], true;
    J.arrayOfSeq [J.Int 2], J.arrayOfSeq [J.Int 2; J.Int 4], false;
    J.arrayOfSeq [J.Int 2], J.arrayOfSeq [J.Int 4], false;
    J.arrayOfSeq [J.Int 2; J.arrayOfSeq [J.Int 2]], J.arrayOfSeq [J.Int 2; J.arrayOfSeq [J.Int 2]], true;
    J.arrayOfSeq [J.Int 2; J.arrayOfSeq [J.Int 2]], J.arrayOfSeq [J.Int 2; J.arrayOfSeq []], false;
    J.objectOfSeq ["asdf", J.Int 2], J.objectOfSeq ["asdf", J.Int 2], true;
    J.objectOfSeq ["asdf", J.Int 2], J.objectOfSeq ["asdf", J.Int 3], false;
    J.objectOfSeq ["fdsa", J.Int 4; "asdf", J.Int 2], J.objectOfSeq ["asdf", J.Int 2; "fdsa", J.Int 4], true;
    J.objectOfSeq ["asdf", J.Int 2], J.objectOfSeq ["asdf", J.Int 2; "fdsa", J.Int 4], false;
    J.objectOfSeq ["a", J.objectOfSeq ["fdsa", J.Int 4; "asdf", J.Int 2]], J.objectOfSeq ["a", J.objectOfSeq ["asdf", J.Int 2; "fdsa", J.Int 4]], true;
]
let equivalenceTestsSeq = tripleSeqToPropertyList equivalenceTests

[<Theory>]
[<PropertyData("equivalenceTestsSeq")>]
let ``JSON equivalence test`` j1 j2 (isTrue:bool) =
    j1 = j2 |> should equal isTrue
