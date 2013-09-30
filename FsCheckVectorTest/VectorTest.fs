module VectorTest

open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open FsCheck
open FsCheck.Commands
open NUnit.Framework

open FsUnit

//model-based checking
type Vector2Actual = Vector<Vector<int>>
type VectorModel = Vector<int> 

let firstVofV f (v : Vector<Vector<int>>) =
    let rec loop = function
        | x when x < v.Length ->
            if v.[x].Length > 0 then x
            else loop (x + 1)
        | _ -> 0
            
    let i = loop 0
    f i 0 

let lastVofV f (v : Vector<Vector<int>>) = 
    let rec loop = function
        | x when x > 0 ->
            if v.[x].Length > 0 then x
            else loop (x - 1)
        | _ -> 0
            
    let i = loop (v.Length - 1)
        
    let j = 
        if v.[i].Length = 0 then 0
        else v.[i].Length - 1
            
    f i j 

let lastV f (v : Vector<Vector<int>>) = 
    let rec loop = function
        | x when x > 0 ->
            if v.[x].Length > 0 then x
            else loop (x - 1)
        | _ -> 0
            
    f (loop (v.Length - 1))

let appendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = c |> update (c.Length - 1)  (append c.[c.Length - 1] (ofSeq elems))
                member x.RunModel m = append m (ofSeq elems)
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "appendInnerMulti: elems = %A" elems }
    }

let tryUpdAppendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = 
                    match (lastVofV tryUpdateNth c) elemForUpd c with
                    | Some x -> 
                        let firstUpdated = (firstVofV updateNth x) elemForUpd x
                        let last = firstUpdated.Length - 1
                        firstUpdated |> update last  (append firstUpdated.[last] (ofSeq elems))
                    | None -> c |> update (c.Length - 1)  (append c.[c.Length - 1] (ofSeq elems))
                member x.RunModel m = 
                    match tryUpdate (m.Length - 1) elemForUpd m with
                    | Some x -> append (update 0 elemForUpd x) (ofSeq elems)
                    | None -> append m (ofSeq elems)
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "appendInnerMulti: elemForUpd = %i, elems = %A" elemForUpd elems }
    }

let conjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = c |> conj (singleton elem)
                member x.RunModel m = conj elem m
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i" elem }
    }

let tryUpdConjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = 
                    match (firstVofV tryUpdateNth c) elemForUpd c with
                    | Some x -> 
                        (lastVofV updateNth x) elemForUpd x
                        |> conj (singleton elem)
                    | None -> c |> conj (singleton elem)
                member x.RunModel m = 
                    match tryUpdate 0 elemForUpd m with
                    | Some x -> 
                        update (x.Length - 1) elemForUpd x
                        |> conj elem
                    | None -> conj elem m
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i, elemForUpd = %i" elem elemForUpd}
    }

let conjInnerEmpty check = 
    Gen.constant <|
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual c = c |> conj empty
            member x.RunModel m = m
            //member x.Pre m = (length m) > 0
            member x.Post (c,m) = check (c,m) //not (check (c,m))
            override x.ToString() = sprintf "conjInnerEmpty"}

let shrinkInner check = 
    Gen.constant <|
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual c = (lastV update c) ((lastV nth c) c).Initial c
            member x.RunModel m = m |> initial
            member x.Pre m = (length m) > 0
            member x.Post (c,m) = check (c,m) //not (check (c,m))
            override x.ToString() = sprintf "shrinkInner"}

let checkFlatten (a,m) = 
    m = (flatten a |> ofSeq) |@ sprintf "failed flatten, model = %A" m        //main check is that the list is the same

let checkLookup (a, (m : Vector<int>)) =         //no way to distinguish which test failed
    if m.Length > 0 then 
            (last m = (lastVofV nthNth a) a)
            .&. (nth 0 m = (firstVofV nthNth a) a) 
//            .&. (true = false) |@ sprintf "true = false, and m.length= %i" m.Length
//            .&. (true = false) |@ sprintf "and true = false, and m.length= %i" m.Length
    else (true = true) |@ sprintf "true = true"
    .&. (tryLast m = (lastVofV tryNthNth a) a)              //also use other operations
    .&. (tryNth 0 m = (firstVofV tryNthNth a) a)    //to test the correctness of those too

let specVofV genList =   
    { new ISpecification<Vector2Actual, VectorModel> with
        member x.Initial() = ((empty |> conj empty), empty)
        member x.GenCommand _ = Gen.oneof genList }

let ``Grow, check by flatten`` = [conjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); appendInnerMulti(checkFlatten)]
let ``Grow, check by look-up`` = [conjInner1Elem(checkLookup); conjInnerEmpty(checkLookup); appendInnerMulti(checkLookup)]
let ``Grow, Update, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten)]
let ``Grow, Update, Shrink, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten); shrinkInner(checkFlatten)]

[<Test>]
let ``Grow Vector<Vector<'T>>, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by flatten``))
//    Check.VerboseThrowOnFailure (asProperty (specVofV ``Grow, check by flatten``))

[<Test>]
let ``Grow Vector<Vector<'T>>, check by look-up``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by look-up``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, check by flatten``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, Shrink, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, Shrink, check by flatten``))

[<Test>]
let WindowedTest() =
    let testWindowed = 
        gen { let! windowLength = Gen.choose(1,5)
              let! source = Arb.generate<List<int>>
              return ((windowSeq windowLength source), (windowLength, source))
        }
        
    Check.QuickThrowOnFailure   (Prop.forAll  (Arb.fromGen testWindowed)
//    Check.Quick (Prop.forAll  (Arb.fromGen testWindowed)  //will leave NUnit runner green-lit, 
                                                            //even though it encountered a falsifiable
                                                            //and reports falsifiable in runner Text Output
//    Check.VerboseThrowOnFailure (Prop.forAll  (Arb.fromGen testWindowed) 
                (fun (vOfV, (windowLength, source)) -> 
                    let outerLength =
                        if source.Length = 0 then 1
                        else int (Math.Ceiling((float)source.Length/(float)windowLength))
                    (((outerLength = vOfV.Length) |@ sprintf "expected outer length= %i actual outer length= %i" outerLength vOfV.Length
                        .&. (true = false) |@ sprintf "true = false" 
                        .&. (flatten vOfV |> List.ofSeq = source) |@ sprintf "flattend vOfV not equal source list"))
                    |> Prop.classify (source.Length > 0 && outerLength > 0) "windowLength, outerLength"
                    |> Prop.classify (source.Length = 0) "empty"
                    |> Prop.collect (windowLength, outerLength)
                )
    )