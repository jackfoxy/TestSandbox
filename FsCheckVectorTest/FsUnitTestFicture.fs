[<NUnit.Framework.TestFixture>]
module FsUnitTestFicture
//https://github.com/mavnn/FsUnit.TestCase/blob/master/FsUnit.TestCase/Library1.fs

open NUnit.Framework
open FsUnit

[<TestFixtureSetUp>]
let SetUp () =
    printfn "Hey! I'm set up for the fixture"

[<TestFixtureTearDown>]
let TearDown () =
    printfn "Going down for whole fixture..."

[<SetUp>]
let ``I'm run before every test!`` () =
    printfn "Before every test..."

let [<Literal>] one = 1

[<TestCase(one, 2, Result = 3)>]
let Adder x y =
    x + y

let testCaseSource =
    [|
        [| 2; 3; 6 |]
        [| 4; 5; 20 |]
    |]

[<Test>]
[<TestCaseSource("testCaseSource")>]
let Multiplier x y result =
    x * y |> should equal result
