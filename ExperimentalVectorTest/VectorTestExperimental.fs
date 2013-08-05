module VectorTestExperimental

open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open FsCheck
open FsCheck.Transitions
open NUnit.Framework
open NUnitRunner

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

let randIndex forModulo outVectLen =
    if outVectLen = 0 then 0
    else Math.Abs (forModulo % outVectLen)

let randPopulatedIndex forModulo (outVect : Vector<Vector<'T>>) =

    if outVect.Length = 1 then 0
    else
        let rec loop acc i =
            match i with
            | x when x < outVect.Length -> 
                if outVect.[x].Length > 0 then  loop (conj x acc) (x + 1)
                else loop acc (x + 1)
            | _ -> acc
        let populatedRows = loop Vector.empty<int> 0 

        let y = populatedRows.[Math.Abs (forModulo % populatedRows.Length)]
        y

let countToEndIndexedInner index (actual : Vector<Vector<'T>>) =
    let rec loop acc i =
        match i with
        | x when x < index -> loop (acc + actual.[x + 1].Length) (x + 1)
        | _ -> acc
    loop 0 -1

///adapted to append to random interior Vector<'T>
let appendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! forModulo = Arb.generate<int>
          return
            { new ITransition<Vector2Actual,VectorModel>() with
                member x.RunActual (a, m) = update (randIndex forModulo a.Length)  (append a.[randIndex forModulo a.Length] (ofSeq elems)) a
                member x.RunModel (a, m) = 
                    let startCount = countToEndIndexedInner (randIndex forModulo a.Length) a
                    let m' = append (ofSeq (Seq.take startCount m)) (ofSeq elems)
                    append m' (ofSeq (Seq.skip startCount m))
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "appendInnerMulti: elems = %A" elems }
    }

let tryUpdAppendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! elemForUpd = Arb.generate<int>
          return
            { new ITransition<Vector2Actual,VectorModel>() with
                member x.RunActual (a, m) = 
                    match (lastVofV tryUpdateNth a) elemForUpd a with
                    | Some x -> 
                        let firstUpdated = (firstVofV updateNth x) elemForUpd x
                        let last = firstUpdated.Length - 1
                        firstUpdated |> update last  (append firstUpdated.[last] (ofSeq elems))
                    | None -> a |> update (a.Length - 1)  (append a.[a.Length - 1] (ofSeq elems))
                member x.RunModel (a, m) = 
                    match tryUpdate (m.Length - 1) elemForUpd m with
                    | Some x -> append (update 0 elemForUpd x) (ofSeq elems)
                    | None -> append m (ofSeq elems)
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "appendInnerMulti: elemForUpd = %i, elems = %A" elemForUpd elems }
    }

let conjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          return
            { new ITransition<Vector2Actual,VectorModel>() with
                member x.RunActual (a, m) = a |> conj (singleton elem)
                member x.RunModel (a, m) = conj elem m
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i" elem }
    }

let tryUpdConjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          let! elemForUpd = Arb.generate<int>
          return
            { new ITransition<Vector2Actual,VectorModel>() with
                member x.RunActual (a, m) = 
                    match (firstVofV tryUpdateNth a) elemForUpd a with
                    | Some x -> 
                        (lastVofV updateNth x) elemForUpd x
                        |> conj (singleton elem)
                    | None -> a |> conj (singleton elem)
                member x.RunModel (a, m) = 
                    match tryUpdate 0 elemForUpd m with
                    | Some x -> 
                        update (x.Length - 1) elemForUpd x
                        |> conj elem
                    | None -> conj elem m
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i, elemForUpd = %i" elem elemForUpd}
    }

let conjInnerEmpty check = 
    Gen.constant <|
        { new ITransition<Vector2Actual,VectorModel>() with
            member x.RunActual (a, m) = a |> conj empty
            member x.RunModel (a, m) = m
            member x.Post (a, m) = check (a, m) //not (check (c,m))
            override x.ToString() = sprintf "conjInnerEmpty"}

///adapted to shrink random interior Vector<'T>
let shrinkInner check = 
    gen { let! forModulo = Arb.generate<int>
          return
        { new ITransition<Vector2Actual,VectorModel>() with
            member x.RunActual (a, m) = update (randPopulatedIndex forModulo a) a.[randPopulatedIndex forModulo a].Initial a
            member x.RunModel (a, m) = 
                let startCount = countToEndIndexedInner (randPopulatedIndex forModulo a) a
                append (ofSeq (Seq.take (startCount - 1) m)) (ofSeq (Seq.skip startCount m))
            member x.Pre (a, m) = (length m) > 0
            member x.Post (a ,m) = check (a, m) //not (check (c,m))
            override x.ToString() = sprintf "conjInnerEmpty"}
    }

let checkFlatten (a, m) : Property = 
    (m = (flatten a |> ofSeq)) |@ sprintf "failed flatten, model = %A" m  //main check is that the list is the same

let checkLookup (a, (m : Vector<int>)) =         //no way to distinguish which test failed
    
    if m.Length > 0 then 
            (last m = (lastVofV nthNth a) a)
            .&. (nth 0 m = (firstVofV nthNth a) a) 
            //.&. (true = false) |@ sprintf "true = false, and m.length= %i" m.Length
    else (true = true) |@ sprintf "true = true"
    .&. (tryLast m = (lastVofV tryNthNth a) a)              //also use other operations
    .&. (tryNth 0 m = (firstVofV tryNthNth a) a)    //to test the correctness of those too

let specVofV genList =   
    { new ITransSpecification<Vector2Actual, VectorModel> with
        member x.Initial() = ((empty |> conj empty), empty)
        member x.GenTransition (_, _) = Gen.oneof genList }

let ``Grow, check by flatten`` = [conjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); appendInnerMulti(checkFlatten)]
let ``Grow, check by look-up`` = [conjInner1Elem(checkLookup); conjInnerEmpty(checkLookup); appendInnerMulti(checkLookup)]
let ``Grow, Update, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten)]
let ``Grow, Update, Shrink, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten); shrinkInner(checkFlatten)]

[<Test>]
let ``Grow Vector<Vector<'T>>, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by flatten``))
//    Check.VerboseThrowOnFailure (asProperty specVector2)

[<Test>]
let ``Grow Vector<Vector<'T>>, check by look-up``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by look-up``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, check by flatten``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, Shrink, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, Shrink, check by flatten``))