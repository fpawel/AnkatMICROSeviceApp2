[<AutoOpen>]
module Utils
open System
open System.Reflection
open System.IO


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

