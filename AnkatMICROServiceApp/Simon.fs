module Simon

module Crc =

    let crcx = [|
        0x0000us;  0x1021us;  0x2042us;  0x3063us;  0x4084us;  0x50a5us;  0x60c6us;  0x70e7us;
        0x8108us;  0x9129us;  0xa14aus;  0xb16bus;  0xc18cus;  0xd1adus;  0xe1ceus;  0xf1efus;
        0x1231us;  0x0210us;  0x3273us;  0x2252us;  0x52b5us;  0x4294us;  0x72f7us;  0x62d6us;
        0x9339us;  0x8318us;  0xb37bus;  0xa35aus;  0xd3bdus;  0xc39cus;  0xf3ffus;  0xe3deus;
        0x2462us;  0x3443us;  0x0420us;  0x1401us;  0x64e6us;  0x74c7us;  0x44a4us;  0x5485us;
        0xa56aus;  0xb54bus;  0x8528us;  0x9509us;  0xe5eeus;  0xf5cfus;  0xc5acus;  0xd58dus;
        0x3653us;  0x2672us;  0x1611us;  0x0630us;  0x76d7us;  0x66f6us;  0x5695us;  0x46b4us;
        0xb75bus;  0xa77aus;  0x9719us;  0x8738us;  0xf7dfus;  0xe7feus;  0xd79dus;  0xc7bcus;
        0x48c4us;  0x58e5us;  0x6886us;  0x78a7us;  0x0840us;  0x1861us;  0x2802us;  0x3823us;
        0xc9ccus;  0xd9edus;  0xe98eus;  0xf9afus;  0x8948us;  0x9969us;  0xa90aus;  0xb92bus;
        0x5af5us;  0x4ad4us;  0x7ab7us;  0x6a96us;  0x1a71us;  0x0a50us;  0x3a33us;  0x2a12us;
        0xdbfdus;  0xcbdcus;  0xfbbfus;  0xeb9eus;  0x9b79us;  0x8b58us;  0xbb3bus;  0xab1aus;
        0x6ca6us;  0x7c87us;  0x4ce4us;  0x5cc5us;  0x2c22us;  0x3c03us;  0x0c60us;  0x1c41us;
        0xedaeus;  0xfd8fus;  0xcdecus;  0xddcdus;  0xad2aus;  0xbd0bus;  0x8d68us;  0x9d49us;
        0x7e97us;  0x6eb6us;  0x5ed5us;  0x4ef4us;  0x3e13us;  0x2e32us;  0x1e51us;  0x0e70us;
        0xff9fus;  0xefbeus;  0xdfddus;  0xcffcus;  0xbf1bus;  0xaf3aus;  0x9f59us;  0x8f78us;
        0x9188us;  0x81a9us;  0xb1caus;  0xa1ebus;  0xd10cus;  0xc12dus;  0xf14eus;  0xe16fus;
        0x1080us;  0x00a1us;  0x30c2us;  0x20e3us;  0x5004us;  0x4025us;  0x7046us;  0x6067us;
        0x83b9us;  0x9398us;  0xa3fbus;  0xb3daus;  0xc33dus;  0xd31cus;  0xe37fus;  0xf35eus;
        0x02b1us;  0x1290us;  0x22f3us;  0x32d2us;  0x4235us;  0x5214us;  0x6277us;  0x7256us;
        0xb5eaus;  0xa5cbus;  0x95a8us;  0x8589us;  0xf56eus;  0xe54fus;  0xd52cus;  0xc50dus;
        0x34e2us;  0x24c3us;  0x14a0us;  0x0481us;  0x7466us;  0x6447us;  0x5424us;  0x4405us;
        0xa7dbus;  0xb7faus;  0x8799us;  0x97b8us;  0xe75fus;  0xf77eus;  0xc71dus;  0xd73cus;
        0x26d3us;  0x36f2us;  0x0691us;  0x16b0us;  0x6657us;  0x7676us;  0x4615us;  0x5634us;
        0xd94cus;  0xc96dus;  0xf90eus;  0xe92fus;  0x99c8us;  0x89e9us;  0xb98aus;  0xa9abus;
        0x5844us;  0x4865us;  0x7806us;  0x6827us;  0x18c0us;  0x08e1us;  0x3882us;  0x28a3us;
        0xcb7dus;  0xdb5cus;  0xeb3fus;  0xfb1eus;  0x8bf9us;  0x9bd8us;  0xabbbus;  0xbb9aus;
        0x4a75us;  0x5a54us;  0x6a37us;  0x7a16us;  0x0af1us;  0x1ad0us;  0x2ab3us;  0x3a92us;
        0xfd2eus;  0xed0fus;  0xdd6cus;  0xcd4dus;  0xbdaaus;  0xad8bus;  0x9de8us;  0x8dc9us;
        0x7c26us;  0x6c07us;  0x5c64us;  0x4c45us;  0x3ca2us;  0x2c83us;  0x1ce0us;  0x0cc1us;
        0xef1fus;  0xff3eus;  0xcf5dus;  0xdf7cus;  0xaf9bus;  0xbfbaus;  0x8fd9us;  0x9ff8us;
        0x6e17us;  0x7e36us;  0x4e55us;  0x5e74us;  0x2e93us;  0x3eb2us;  0x0ed1us;  0x1ef0us |]

    let shift x = 
        (x <<< 8) ^^^ crcx.[int (x >>> 8) ]

    let get (bytes : byte seq) =
        Seq.fold (fun a x -> 
            (shift a) ^^^ ( uint16 x)
            ) 0xFFFFus bytes
        |> shift
        |> shift

    let get1 xs = 
        let x = get xs
        [|  byte (x >>> 8)
            byte x |]
    // crc [| for n = 1uy to 8uy do yield n |] = 38805us



[<AutoOpen>]
module private Help =
    

    let checkResponse cmdCount (responseBytes:byte [])   = 
        let len = Array.length responseBytes 
        if len=0 then Err "не отвечает"
        
        elif len<7 then 
            sprintf "дина ответа должна быть не менее 7, фактически %d" len             
            |> Err else
        if  responseBytes.[0] <> 0x5Auy || (responseBytes.[1] >>> 4) <> 0x05uy then
            Err "несовпадение маркера 0x5A5х"  else
        if len - 2 <> int responseBytes.[2] then
            responseBytes.[2]
            |> sprintf "несовпадение длины ответа %d cо значением третьего байта ответа %d" len 
            |> Err else
        if cmdCount - 1uy <> responseBytes.[3] then 
            responseBytes.[3] 
            |> sprintf "несовпадение кода комманды запроса %d и ответа %d" (cmdCount - 1uy)
            |> Err else
        if len = 7 && responseBytes.[4] <> 0uy then
            sprintf "прибор вернул код ошибки %d" responseBytes.[4]
            |> Err
        else Ok ()

    

    let makeRequest cmdCount cmdCode reqDataBytes =
        
        let sendStrLen = Seq.length reqDataBytes
        let sz = 7uy + byte sendStrLen
        let xs1 = [|   
            0xA5uy
            0xA0uy
            sz - 2uy
            cmdCount
            cmdCode |]
        let xs = Array.append xs1 reqDataBytes
        let crc = Crc.get xs
        Array.append xs (Crc.get1 xs)

    let mutable cmdCount = 0uy

    let getResponse what cmdCode reqDataBytes formatResult parseResponse  = async{
        let requestBytes = makeRequest cmdCount cmdCode reqDataBytes
        let config = AppConfig.get()
        let port : Comport.Config = 
            {   Timing = config.SimonTiming
                PortName = config.PortName
                Description = "SIMON" }

        let! response,result = 
            Comport.Async.getProtocolResponse 
                port requestBytes 
                (checkResponse cmdCount) 
                parseResponse

        let level, strResult, needLog = 
            match result with
            | Ok x -> Trace.Info, formatResult x, Comport.traceComport
            | Err e -> Trace.Error, e, true

        if needLog then             
            Trace.write level "%s %s %s, ком.%d%s" 
                port.PortName "SIMON" what cmdCode
                (if strResult = "" then "" else " ==> " + strResult )

        match response with
        | Some (Array.NotEmpty true) -> 
            cmdCount <- cmdCount + 1uy
        | _ -> ()

        return result }


let readFlash addy len =
    assert(len>1 && len<33)
    let what = sprintf "FLASH[%d..%d]" addy (addy + len -1 )
    let req = 
        [|  addy >>> 16
            addy >>> 8
            addy
            len - 1 |]
        |> Array.map byte
    getResponse what 0x42 req bytesToStr id


    const unsigned char txd[4] = { addy>>16,  addy>>8, addy, len-1 };
    Adpt().PerformTransfer( 0x42, txd, txd+4 );
    const char * rxd = Adpt().AcceptedData();
    const unsigned rxdLen = Adpt().AcceptedDataSize();
    if(rxdLen!=len) MY_THROW_( MYSPRINTF_ (
        "Ошибка считывания FLASH прибора.\n"
        "Запрошено %d байт\n"
        "Получено %d байт", len, rxdLen));
    std::copy( rxd, rxd+len, p );

    if( CtrlSys::Instance().MustShowFlasLog() )
       MyWCout( MyBuffToStr( p, p+len, "Флэш ", addy, 6 )+"\n", MY_SET_CONSOLE_YELLOW_TEXT);
}