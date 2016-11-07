[<AutoOpen>]
module Utils
open System
open System.Reflection
open System.IO
open System.Text.RegularExpressions

module Array =
    let (|NotEmpty|) x = Array.isEmpty x |> not

module Seq =

    let toStr<'T> delimString conv (collection : 'T seq )  = 
        collection |> Seq.fold( fun acc x ->
            acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""


module Option = 
    let getWith x = function
        | None -> x
        | Some x' -> x'

let executingAssembly =
    try
        Some <| Assembly.GetExecutingAssembly()
    with _ -> 
        None

type Path with
    static member createIfNotExists (x  :string) = 
        if not ( IO.Directory.Exists x ) then
            IO.Directory.CreateDirectory x
            |> ignore
        x

let private appDataPath =
    let a = Environment.GetFolderPath Environment.SpecialFolder.ApplicationData
    let x = IO.Path.Combine(a, "AnkatMicroServiceApp")
    Path.createIfNotExists x

let numberWithLeadZero n = 
    if n < 10 then sprintf "0%d" n else string n

let bytesToStrings bytes =      
    Seq.fold ( fun (acc,i) b ->
        let s = sprintf "%s%X" (if b<0x10uy then "0" else "") b
        if i%16=0 then (s::acc, i+1) else                     
        match acc with 
        | [] -> ( [s], i+1) 
        | [s'] -> ([ s'+" "+s], i+1)
        | s'::acc  -> ((s'+" "+s)::acc, i+1)) ( [], 0 ) bytes |> fst |> List.rev

let bytesToStr bytes = Seq.fold ( fun acc s -> if acc="" then s else acc + " "+s) "" (bytesToStrings bytes)
let (|BytesToStr|) = bytesToStr

let intToHex len x = 
    let x = sprintf "%X" x
    let n = String.length x
    (if n < len then String('0', len-n ) else "") + x

let formatBytesPool16 numbersCount addy0 xs =
        
        xs |> Array.mapi( fun n x -> 
            "" )
        //sprintf "%s : " (intToHex numbersCount n)
   

type Path with
    static member appData = appDataPath

    static member ofDateTime (dateTime : DateTime) =
        let monthName = dateTime.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
        
        Path.Combine
            (   string dateTime.Year, 
                numberWithLeadZero dateTime.Month + "-" + monthName,
                numberWithLeadZero dateTime.Day )

type DateTime with
    
    static member format (spec:string) (x:DateTime) = 
        x.ToString(spec)

    static member toString x = 
        DateTime.format "HH:mm:ss" x

    static member toString1 (x:DateTime) = 
        DateTime.format "HH:mm" x

    static member toString2 (x:DateTime) = 
        DateTime.format "dd.MM.yy HH:mm" x

let version = 
    executingAssembly
    |> Option.map (fun a -> a.GetName().Version)


[<AutoOpen>]
module WinFormsHelp =
    
    open System.Windows.Forms

    type Control with
        static member performThreadSafeAction<'a> (ctrl:Control) (f:unit -> 'a) =
            if ctrl.InvokeRequired then 
                let mutable x : 'a option = None
                ctrl.Invoke <| new  MethodInvoker( fun () -> x <- Some (f()) ) |> ignore
                x.Value
            else f()

        member x.PerformThreadSafeAction f = Control.performThreadSafeAction x f


type Result<'T, 'E> = 
    | Ok of 'T
    | Err of 'E

module Result =
    
    let isErr = function
        | Err _ -> true
        | _      -> false

    let isOk = function
        | Ok _ -> true
        | _      -> false

    let map f = function
        | Ok x -> Ok( f x )
        | Err e -> Err e  

    let mapErr f = function
        | Ok x -> Ok x
        | Err e -> Err ( f e  )

    let bind f = function
        | Ok x ->  f x
        | Err e -> Err e

    let bindErr f = function
        | Ok x ->  Ok x
        | Err e -> f e

    let someErr = function
        | Ok _ ->  None
        | Err e -> Some e

    module Unwrap =         

        let ok = function
            | Ok x -> x
            | Err e -> failwithf "unwraping Err %A as Ok" e

        let err = function
            | Ok x -> failwithf "unwraping Ok %A as Err" x
            | Err e -> e

module Async = 
    let map f x = async {
        let! y = x
        return f y
    }

    let return' x = async {
        return x
    }

let isCmdLineArg (x:string) =
    Regex.Match( Environment.CommandLine.ToLower(), "\\-" + x.ToLower()).Success
