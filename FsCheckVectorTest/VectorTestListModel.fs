module VectorTestExperimental

open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open FsCheck
open FsCheck.Commands
open NUnit.Framework

open FsUnit

//model-based checking
type Vector2Actual = Vector<Vector<int>>
type VectorModel = List<List<int>>

let flattenModel model = List.foldBack (fun (t:List<int>) s -> s @ (List.rev t)) model [] 

let updateModel i elem model = 
    let a = FSharpx.Collections.List.take i model
    let c = FSharpx.Collections.List.skip (i + 1) model
    a @  (List.cons elem c)     
    
let modelNth i modelList = List.head (FSharpx.Collections.List.skip i modelList)

let tryModelNth i modelList = 
    let x = FSharpx.Collections.List.skip i modelList
    if x.IsEmpty then None
    else Some (List.head x)

let firstInModel (model : List<List<int>>) =
    let rec loop (dec : List<List<int>>) =
        match dec with
        | hd::tl when hd.Length > 0 -> hd 
        | hd::tl -> loop tl
        | [] -> List.empty<int>
    
    let firstPop = loop model

    firstPop.Head

let updateFirstInModel upd (model : List<List<int>>) =
    let rec loop count (dec : List<List<int>>) =
        match dec with
        | hd::tl when hd.Length > 0 -> hd, count 
        | hd::tl -> loop (count + 1) tl
        | [] -> List.empty<int>, 0
    
    let firstPop, i = loop 0 model

    updateModel i (upd::firstPop.Tail) model

let tryUpdateFirstOfM upd (model : List<List<int>>) =
    let rec loop count (dec : List<List<int>>) =
        match dec with
        | hd::tl when hd.Length > 0 -> hd, count 
        | hd::tl -> loop (count + 1) tl
        | [] -> List.empty<int>, 0
    
    let firstPop, i = loop 0 model

    if firstPop.Length = 0 then None
    else
        let newL = updateModel 0 upd firstPop
        Some (updateModel i newL model)

let tryFirstInModel (model : List<List<int>>) =
    let rec loop (dec : List<List<int>>) =
        match dec with
        | hd::tl when hd.Length > 0 -> hd 
        | hd::tl -> loop tl
        | [] -> List.empty<int>
    
    let firstPop = loop model

    if firstPop.Length = 0 then None
    else Some firstPop.Head

let tryFirstLofM (model : List<List<int>>) =
    let rec loop (dec : List<List<int>>) =
        match dec with
        | hd::tl when hd.Length > 0 -> Some hd 
        | hd::tl -> loop tl
        | [] -> None
    
    loop model

    
let tryUpdateLastOfM upd (model : List<List<int>>) =
    let rec loop acc lastCount count (dec : List<List<int>>) =
        match dec with
        | hd::tl ->
            if hd.Length > 0  then loop hd count (count + 1) tl
            else loop acc lastCount (count + 1) tl
        | [] -> acc, lastCount
    
    let last, i = loop List.empty<int> 0 0 model

    if last.Length = 0 then None
    else
        let newL = updateModel (last.Length - 1) upd last
        Some (updateModel i newL model)

let appendFirstOfM apnd (model : List<List<int>>) =
    updateModel 0 ((List.rev apnd) @ model.Head) model

let lastInModel (model : List<List<int>>) =
    let rec loop acc (dec : List<List<int>>) =
        match dec with
        | hd::tl ->
            if hd.Length > 0  then loop hd tl
            else loop acc tl
        | [] -> acc
    
    let lastPop = loop List.empty<int> model

    List.nth lastPop (lastPop.Length - 1)

let tryLastInModel (model : List<List<int>>) =
    let rec loop acc (dec : List<List<int>>) =
        match dec with
        | hd::tl ->
            if hd.Length > 0  then loop hd tl
            else loop acc tl
        | [] -> acc
    
    let lastPop = loop List.empty<int> model

    if lastPop.IsEmpty then None
    else Some (List.nth lastPop (lastPop.Length - 1))

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

        populatedRows.[Math.Abs (forModulo % populatedRows.Length)]

let randPopulatedModelIndex forModulo (outModel : List<List<int>>) =

    if outModel.Length = 1 then 0
    else
        let rec loop acc i  (dec : List<List<int>>) =
            match dec, i with
            | hd::tl, x -> 
                if hd.Length > 0 then  loop (conj x acc) (x + 1) tl
                else loop acc (x + 1) tl
            | [], _ -> acc
        let populatedRows = loop Vector.empty<int> 0 outModel 

        let i = Math.Abs (((Math.Abs(forModulo % populatedRows.Length)) - (populatedRows.Length - 1) ))
        populatedRows.[i]

let countToEndIndexedInner index (actual : Vector<Vector<'T>>) =
    let rec loop acc i =
        match i with
        | x when x < index -> loop (acc + actual.[x + 1].Length) (x + 1)
        | _ -> acc
    loop 0 -1

///append to random interior Vector<'T>
let appendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! forModulo = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual a = 
                    let i = randIndex forModulo a.Length
                    update i (append a.[i] (ofSeq elems)) a
                member x.RunModel m = 
                    let i = Math.Abs ((randIndex forModulo m.Length) - (m.Length - 1))
                    let a = List.ofSeq elems |> List.rev
                    updateModel i (a @ (modelNth i m)) m
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "appendInnerMulti: elems = %A" elems }
    }

let tryUpdAppendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual a = 
                    match (lastVofV tryUpdateNth a) elemForUpd a with
                    | Some x -> 
                        let firstUpdated = (firstVofV updateNth x) elemForUpd x
                        let last = firstUpdated.Length - 1
                        firstUpdated |> update last  (append firstUpdated.[last] (ofSeq elems))
                    | None -> update (a.Length - 1)  (append a.[a.Length - 1] (ofSeq elems)) a
                member x.RunModel m = 
                    match tryUpdateFirstOfM elemForUpd m with
                    | Some x -> 
                        (tryUpdateLastOfM elemForUpd x).Value
                        |> appendFirstOfM (List.ofSeq elems) 
                    | None -> updateModel 0 (List.ofSeq elems |> List.rev) m
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "appendInnerMulti: elemForUpd = %i, elems = %A" elemForUpd elems }
    }

let conjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual a = a |> conj (singleton elem)
                member x.RunModel m = List.cons [elem] m
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i" elem }
    }

let tryUpdConjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual a = 
                    match (firstVofV tryUpdateNth a) elemForUpd a with
                    | Some x -> 
                        (lastVofV updateNth x) elemForUpd x
                        |> conj (singleton elem)
                    | None -> a |> conj (singleton elem)
                member x.RunModel m = 
                    match tryUpdateLastOfM elemForUpd m with
                    | Some x ->
                        updateFirstInModel elemForUpd x
                        |> List.cons [elem]
                    | None -> m |> List.cons [elem]
                member x.Post (a, m) = check (a, m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i, elemForUpd = %i" elem elemForUpd}
    }

let conjInnerEmpty check = 
    Gen.constant <|
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual a = a |> conj empty
            member x.RunModel m = m |> List.cons List.empty
            member x.Post (a, m) = check (a, m) //not (check (c,m))
            override x.ToString() = sprintf "conjInnerEmpty"}

///shrink random interior Vector<'T>
let shrinkInner check = 
    gen { let! forModulo = Arb.generate<int>
          return
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual a = 
                let i = randPopulatedIndex forModulo a
                update i a.[i].Initial a
            member x.RunModel m = 
                let i = randPopulatedModelIndex forModulo m
                updateModel i (List.tail (modelNth i m)) m
            member x.Pre m = (Seq.length (flattenModel m)) > 0
            member x.Post (a ,m) = check (a, m) //not (check (c,m))
            override x.ToString() = sprintf "shrinkInner"}
    }

let lastEachPopulatedVect (v : Vector<Vector<int>>) = 
    Vector.fold (fun s (t : Vector<int>) -> if t.Length > 0 then t.[t.Length - 1]::s else s) [] v

let lastEachPopulatedModel (m : List<List<int>>) =
    List.foldBack (fun (t : List<int>) s -> if t.Length > 0 then t.Head::s else s) m []

let populatePosVect (v : Vector<Vector<int>>) = 
    Vector.fold (fun s (t : Vector<int>) -> if t.Length = 0 then "E"::s else "P"::s) [] v

let populatePosModel (m : List<List<int>>) =
    List.foldBack (fun (t : List<int>) s -> if t.Length = 0 then "E"::s else "P"::s) m []

//let checkLookup (a, (m : List<List<int>>)) =
let check (a, (m : List<List<int>>)) =
    let tryFirstM m = 
        match tryModelNth 0 m with
        | None -> None
        | Some (x : 'a list) -> 
            if x.Length = 0 then None
            else Some (List.head x)

    if (Seq.length (flattenModel m)) > 0 then 
            (firstInModel m = (lastVofV nthNth a) a) |@ sprintf "firstM m = lastVofV a, actual = %A model = %A" a m                     //nthNth not otherwise tested
            .&. (lastInModel m  = (firstVofV nthNth a) a) |@ sprintf "lastInModel m = firstVofV a, actual = %A model = %A" a m
    else (flatten a |> List.ofSeq = flattenModel m) |@ sprintf "flatten, actual = %A model = %A" a m                                    //elements and order match
    .&. (lastEachPopulatedVect a = lastEachPopulatedModel m) |@ sprintf "last elem of each populated row, actual = %A model = %A" a m   //elements divided into correct rows
    .&. (populatePosVect a = populatePosModel m) |@ sprintf "pattern of populated rows, actual = %A model = %A" a m                     //pattern of populated rows
    .&. (true = false) |@ sprintf "true = false"                                                                                      //also reports succeeding properties, to be debugged                                              
    .&. (tryFirstInModel m = (lastVofV tryNthNth a) a) |@ sprintf "tryFirstM m = lastVofV a, actual = %A model = %A" a m                //tryNthNth not otherwise tested
    .&. (tryLastInModel m = (firstVofV tryNthNth a) a) |@ sprintf "tryLastInModel m = firstVofV a, actual = %A model = %A" a m     

let specVofV genList =   
    { new ISpecification<Vector2Actual, VectorModel> with
        member x.Initial() = ((empty |> conj empty), List.empty |> List.cons List.empty)
        member x.GenCommand _ = Gen.oneof genList }

let ``Grow, check`` = [conjInner1Elem(check); conjInnerEmpty(check); appendInnerMulti(check)]
let ``Grow, Update, check`` = [tryUpdConjInner1Elem(check); conjInnerEmpty(check); tryUpdAppendInnerMulti(check)]
let ``Grow, Update, Shrink, check`` = [tryUpdConjInner1Elem(check); conjInnerEmpty(check); tryUpdAppendInnerMulti(check); shrinkInner(check)]

[<Test>]
let ``Grow Vector<Vector<'T>>, check``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, check``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, check``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, Shrink, check``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, Shrink, check``))