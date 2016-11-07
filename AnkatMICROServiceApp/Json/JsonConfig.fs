module Json.Config

open System
open System.IO

open Json.Serialization


    


let private retDummy<'a> (filename, dummy  : unit -> 'a) errorText =    
    dummy(), Some (sprintf "ошибка файла конфигурации json \"%s\": %s" filename errorText ) 

let read filename dummy =    
    let path =  IO.Path.Combine( IO.Path.appData, filename)
    let retDummy = retDummy (filename, dummy)
    let x, e = 
        if IO.File.Exists path then 
            try
                match parse (IO.File.ReadAllText(path)) with
                | Ok x -> x, None
                | Err x -> retDummy x
            with e -> retDummy <| sprintf "%A" e
        else
            retDummy "файл не существует"
    match e with
    | Some e -> Trace.error "%s" e
    | _ -> ()
    x

let write filename x' = 
    let path =  IO.Path.Combine( IO.Path.appData, filename)
    let x = stringify x'
    let r =
        try
            File.WriteAllText(filename, x)
            Ok ()
        with e ->
            Err <| sprintf "oшибка записи файла конфигурации json %A: %A" filename e 

    match r with
    | Err e -> Trace.error "%s" e
    | _ -> ()

let create filename dummy =         
    let mutable config = read filename dummy
    let set v = 
        config <- v
        write filename config
    let get () = config
    get, set