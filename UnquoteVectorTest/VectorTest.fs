module UnquoteVectorTest

open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open Swensen.Unquote
open NUnit.Framework
open FsUnit

type Class1() = 
    member this.X = "F#"

[<Test>]
let ``check 1``() =
    //Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check``))

    test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>

[<Test>]
let ``check 2``() =
    //Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check``))
    
    test <@ ([0; 1; 2; 3] |> List.map ((+) 1)) = [0 + 1..3 + 1] @>

[<Test>]
let ``Vector<Vector<'T>>, check 1``() =
    //Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check``))
    
    test <@ ([0; 1; 2; 3] |> List.map ((+) 1))  = (Vector.empty |> Vector.conj 0 |> Vector.conj 1 |> Vector.conj 2 |> Vector.conj 3 |> List.ofSeq) @>

[<Test>]
//It's more interesting to build failing Unquote tests and look at the results than use if for passing tests.
let ``Vector<Vector<'T>>, windowSeq``() =
    test<@ [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10;] = 
        ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9; ]
         |> Vector.windowSeq 3
         |> Vector.flatten
         |> List.ofSeq)  @>