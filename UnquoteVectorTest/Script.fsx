
#if INTERACTIVE
//    #r "file.dll";;        Reference (dynamically load) the given DLL
//    #I "path";;            Add the given search path for referenced DLLs
//    #load "file.fs" ...;;  Load the given file(s) as if compiled and referenced
//    #time ["on"|"off""];;  Toggle timing on/off
//    #help;;                Display help
//    #quit;;                Exit
//    fsi path by default C:\Users\Jack\AppData\Local\Temp\
#load @"C:\FsEye\FsEye.fsx"
#I "C:\Packages" 
#r "FSharpx.Core.1.8.37\lib\40\Fsharpx.Core.dll" 
#r "System.Xml.Linq.dll"
#r @"Unquote.2.2.2\lib\net40\Unquote.dll"
#r @"NUnit.2.6.2\lib\nunit.framework.dll"
#endif
    
open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open Swensen.Unquote
open NUnit.Framework

[<Test>]
//It's more interesting to build failing Unquote tests and look at the results than use if for passing tests.
let ``Vector<Vector<'T>>, windowSeq``() =
    test<@ [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10;] = 
        ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9; ]
         |> Vector.windowSeq 3
         |> Vector.flatten
         |> List.ofSeq)  @>

``Vector<Vector<'T>>, windowSeq``