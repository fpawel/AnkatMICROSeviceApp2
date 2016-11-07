module Comport

open System
open System.IO.Ports





type Timing =
    {   Timeout     : int
        Delay       : int
        Chartime    : int
        RepeatCount : int
        BaudRate    : int  }  
        
    static member dummy() = {   
        Timeout = 1000
        Delay = 0
        Chartime = 20
        RepeatCount = 0
        BaudRate = 9600  }
    
type Config =
    {   PortName    : string
        Description : string 
        Timing      : Timing }  
        
    static member dummy() = {   
        PortName    = ""
        Description = "-" 
        Timing      = Timing.dummy()  }

    

module Ports =
    let mutable serialPorts : Map<string,SerialPort > = Map.empty
    
    let getPort config = 
        match serialPorts.TryFind config.PortName with
        | Some port -> Ok port
        | _ ->
            try
                let newport = new SerialPort(config.PortName)
                serialPorts <- Map.add config.PortName newport serialPorts
                Ok newport
            with e -> Err e.Message               

    let getPortByName portName =        
        match serialPorts |> Map.tryFind portName with
        | Some port -> Ok port
        | _ ->  
            try
                new SerialPort(portName) |> Ok
            with e -> Err e.Message

    let removeFailedPort config = 
        serialPorts <- serialPorts |> Map.filter( fun p' _ -> config.PortName <> p')
        getPort config

    let getPorts =  
        serialPorts |> Map.toList |> List.map( fun( portName, _) -> portName )

    let getPortsNames = ""::(SerialPort.GetPortNames() |> Array.toList)
    let closeOpenedPorts( pred ) =  
        serialPorts 
        |> Map.filter( fun portName port -> 
            port.IsOpen 
            &&  SerialPort.GetPortNames() |> Set.ofArray |> Set.contains portName 
            && (pred portName) )
        |> Map.iter ( fun _ port -> 
            //Trace.debug "Закрытие порта %s" port.PortName
            port.Close() )
        serialPorts <- Map.empty

let traceComport = isCmdLineArg "trace-comport"

[<AutoOpen>]
module private Helpers2 =     
    
    let formatError (portName : string) (msg : string) txd = 
        if msg.Contains portName then msg else 
        sprintf "COM порт %A, %s%s" 
            portName msg 
            (if traceComport then sprintf ", %s" <| bytesToStr txd else "")

    let tickcount() = System.Environment.TickCount

    let sleep (n:int) = 
        let x0 = tickcount()
        while tickcount() - x0 < n  do
            System.Threading.Thread.Sleep 10

    let discardBuffers (port:SerialPort) = 
        port.DiscardInBuffer()
        port.DiscardOutBuffer()

    let private doApply (sets : Config) (port : SerialPort) = 
        port.Close()
        port.PortName <- sets.PortName
        port.BaudRate <- sets.Timing.BaudRate     
        port.Open()
        discardBuffers port
        Ok port

    let applyPort sets = 
        
        if sets.PortName |> String.IsNullOrEmpty then
            (sprintf "%A : не установлен СОМ порт" sets.Description) |> Err
        elif SerialPort.GetPortNames() |> Set.ofArray |> Set.contains sets.PortName |> not then 
            (sprintf "Недопустимое имя СОМ порта %A" sets.PortName ) |> Err else
        match Ports.getPort sets with
        | Err x -> Err x
        | Ok port ->
            if port.IsOpen && port.PortName<>sets.PortName && port.BaudRate<>sets.Timing.BaudRate then Ok port else                
            try doApply sets port with e1 ->
            try
                match Ports.removeFailedPort sets with
                | Err x -> Err x
                | Ok port -> doApply sets port
            with e2 ->
                if e1.Message <> e2.Message then sprintf "%s, %s" e1.Message e2.Message else e1.Message
                |> sprintf "Не удалось открыть порт %s: %s" sets.PortName
                |> Err 

    let writeToPort (port:SerialPort) (txd:byte seq)  =        
        let txd = Seq.toArray txd
        port.Write( txd, 0, txd.Length)    

    let checkCondWithTimeout timeout cond  work = 
        let t0 = tickcount()
        let rec loop() = 
            work()
            if  (cond() |> not) &&  (tickcount() - t0 < timeout) then 
                loop() 
        loop()


let tryApplyPort = applyPort

let write sets (txd:byte seq)  =
    let fail error = formatError sets.PortName error txd |> Some
    try
        match tryApplyPort sets with
        | Err error -> fail error
        | Ok port ->
            if traceComport then bytesToStr txd |> Trace.debug "%s <== %s" sets.PortName 
            let txd = Seq.toArray txd
            port.Write( txd, 0, txd.Length)
            None
    with e ->
        fail e.Message



[<AutoOpen>]
module private Helpers3 =   

    type Request =  {
        serial : SerialPort
        config : Config
        txd : byte []
        n : int }


    let rec loopRecive req recived = 
        let hasBytes() = req.serial.BytesToRead>0
        checkCondWithTimeout req.config.Timing.Timeout hasBytes ( fun() -> sleep 1 )
        if req.serial.BytesToRead=0 then Ok recived else
        let rxdlen = req.serial.BytesToRead
        let oldlen = recived |> Array.length
        let recived = [| yield! recived; yield! Array.create rxdlen 0uy |]            
        if req.serial.Read(recived, oldlen, rxdlen) <> rxdlen then 
            Err <| sprintf "error reading comport %s, %s" req.serial.PortName (bytesToStr req.txd)
        else
            checkCondWithTimeout (max req.config.Timing.Chartime 1) hasBytes ( fun() -> sleep 5 )
            if req.serial.BytesToRead=0 then 
                Ok recived 
            else 
                loopRecive req recived

    let rec loopTransmit req =  
        writeToPort req.serial req.txd
        let tickStart = tickcount()
        let response = loopRecive req [||]
        let ms = tickcount()-tickStart
        if traceComport then
            let whatRecived = 
                match response with
                | Err e -> e
                | Ok [||] -> "нет ответа"
                | Ok rxd -> bytesToStr rxd 
            Trace.debug "%s, %s ==> %s, %d мс" req.config.PortName (bytesToStr req.txd) whatRecived ms
        response 
        |> Result.bind( function
            | [||] when req.n < req.config.Timing.RepeatCount -> 
                loopTransmit { req with n = req.n + 1 }
            | [||] -> Ok [||] 
            | rxd ->
                sleep req.config.Timing.Delay
                Ok rxd )

let getResponse port requestBytes : Result<byte[], string>=         
    try
        match tryApplyPort port with
        | Err error -> formatError port.PortName error requestBytes |> Err
        | Ok serial ->
            loopTransmit { config = port; serial = serial; n =  0; txd =  requestBytes }            
    with e ->
        formatError port.PortName e.Message requestBytes |> Err

let getProtocolResponse port requestBytes checkResp parseResp =
    match getResponse port requestBytes with
    | Err error -> None, Err error
    | Ok responsedBytes -> 
        Some responsedBytes, 
            checkResp responsedBytes  
            |> Result.bind parseResp

module Async = 
    
    let write sets (txd:byte seq)  =
        let fail error = formatError sets.PortName error txd |> Some
        try
            match tryApplyPort sets with
            | Err error -> 
                fail error
                |> Async.return'
            | Ok port ->
                if traceComport then bytesToStr txd |> Trace.debug "%s <== %s" sets.PortName 
                let txd = Seq.toArray txd
                port.BaseStream.AsyncWrite( txd, 0, txd.Length)
                |> Async.map (fun _ -> None)
        with e ->
            fail e.Message
            |> Async.return'

    [<AutoOpen>]
    module private Helpers =  

        let checkCondWithTimeout timeout cond work = 
            let t0 = tickcount()
            let rec loop = async{
                do! work
                if  (cond() |> not) &&  (tickcount() - t0 < timeout) then 
                    do! loop 
                }
            loop

        let rec loopRecive req recived = async{ 
            let hasBytes() = req.serial.BytesToRead>0
            do! checkCondWithTimeout req.config.Timing.Timeout hasBytes (Async.Sleep 1)
            if req.serial.BytesToRead=0 then return Ok recived else
            let rxdlen = req.serial.BytesToRead
            let oldlen = recived |> Array.length
            let recived = [| yield! recived; yield! Array.create rxdlen 0uy |]            
            let! readedBytesCount = req.serial.BaseStream.AsyncRead(recived, oldlen, rxdlen)
            if readedBytesCount <> rxdlen then 
                return 
                    bytesToStr req.txd
                    |> sprintf "error reading comport %s, %s" req.serial.PortName 
                    |> Err
            else
                do! checkCondWithTimeout req.config.Timing.Timeout hasBytes (Async.Sleep 5)
                if req.serial.BytesToRead=0 then 
                    return Ok recived 
                else 
                    return! loopRecive req recived }

        let writeToPort (port:SerialPort) (txd:byte seq)  =        
            let txd = Seq.toArray txd
            port.BaseStream.AsyncWrite( txd, 0, txd.Length)    

        let rec loopTransmit req =  async{
            do! writeToPort req.serial req.txd
            let tickStart = tickcount()
            let! response = loopRecive req [||]
            let ms = tickcount()-tickStart
            if traceComport then
                let whatRecived = 
                    match response with
                    | Err e -> e
                    | Ok [||] -> "нет ответа"
                    | Ok rxd -> bytesToStr rxd 
                Trace.debug "%s, %s ==> %s, %d мс" req.config.PortName (bytesToStr req.txd) whatRecived ms
            match response with
            | Ok [||] when req.n < req.config.Timing.RepeatCount -> 
                return! loopTransmit { req with n = req.n + 1 }
            | Ok [||] -> return Ok [||] 
            | Ok rxd -> 
                do! Async.Sleep req.config.Timing.Delay
                return Ok rxd
            | Err _ as err -> return err }

    let getResponse port requestBytes : Async< Result<byte[], string> > = async {
       
        try
            match tryApplyPort port with
            | Err error -> 
                return 
                    requestBytes 
                    |> formatError port.PortName error 
                    |> Err
            | Ok serial ->
                return! loopTransmit { config = port; serial = serial; n =  0; txd =  requestBytes }            
        with e ->
            return 
                requestBytes 
                |> formatError port.PortName e.Message 
                |> Err 
        }

    let getProtocolResponse port requestBytes checkResp parseResp = async{ 
        let! response = getResponse port requestBytes
        match response with
        | Err error -> return None, Err error
        | Ok responsedBytes -> 
            return
                Some responsedBytes, 
                    checkResp responsedBytes  
                    |> Result.bind parseResp }