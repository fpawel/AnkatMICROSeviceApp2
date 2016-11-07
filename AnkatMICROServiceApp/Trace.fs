module Trace

open System
open System.IO
open System.Drawing
open System.Diagnostics

type Level = 
    | Error
    | Warn
    | Info
    | Debug
    static member toString (x:Level) = 
        (sprintf "%A" x).ToUpper()



module private Help =
    open Ionic.Zip

    let zipArchFileName = 
        Path.Combine( Path.appData, "trace.zip")

    let currentFileName() =
            Path.Combine(Path.ofDateTime DateTime.Now, "trace.log" )

    let initialize(mem : MemoryStream) = 
        if File.Exists zipArchFileName then
            use zip = ZipFile.Read zipArchFileName 
            let currentFileName = currentFileName() 
            if zip.ContainsEntry currentFileName then                
                zip.[currentFileName].Extract(mem)
        mem
        
    let memoryStream = 
        initialize <| new MemoryStream()

    let streamWriter = 
        new StreamWriter(memoryStream)
        
    let syncObject = obj()
        
    let write level message =
        lock syncObject <| fun () ->
            fprintfn streamWriter "%s|%s|%s" (DateTime.toString DateTime.Now) (Level.toString level) message 

    let save() = 
        lock syncObject <| fun () ->
            streamWriter.Flush()            
            if memoryStream.Position > 0L then
                let currentFileName = currentFileName() 
                use zip = new ZipFile( zipArchFileName )
                if zip.ContainsEntry currentFileName then
                    zip.RemoveEntry currentFileName                 
                memoryStream.Position <- 0L
                let _ = zip.AddEntry(currentFileName, memoryStream)
                zip.Save()            

    open System.Net.Mail

    let saveEmailErrorReportToFile() = 
        let mail = new MailMessage("x@y.z", " info@analitpribor-smolensk.ru, market@analitpribor-smolensk.ru")
        mail.Subject <- "сбой сервисного ПО Анкат-7664Микро"
        mail.Body <- ""
        mail.Attachments.Add(new Attachment(zipArchFileName))
        let id = Guid.NewGuid()
        
        let tempFolder = Path.Combine(Path.GetTempPath(), string <| Guid.NewGuid())

        if not <| Directory.Exists(tempFolder) then        
            Directory.CreateDirectory tempFolder   
            |> ignore     

        use client = 
            new SmtpClient( 
                DeliveryMethod = SmtpDeliveryMethod.SpecifiedPickupDirectory, 
                UseDefaultCredentials = true,
                PickupDirectoryLocation = tempFolder)
        client.Send(mail)        
         

        let pathToErrEmlFile = 
            let dateStr = DateTime.Now.ToString("dd-MM-yy_HH-mm-ss-ffffff")
            let filenameStr = sprintf "%s-ankat-micro-serviceapp-error.eml" dateStr
            let strFolderPath = 
                Path.Combine( Path.appData,  "Errors-emails" )
                |> Path.createIfNotExists
            Path.Combine( strFolderPath,  filenameStr )

        if File.Exists pathToErrEmlFile then
            File.Delete pathToErrEmlFile

        // tempFolder should contain 1 eml file
        match Directory.GetFiles(tempFolder) with
        | [|fileName|] -> 
            File.Move(fileName, pathToErrEmlFile)
        | _ -> ()
        DirectoryInfo(tempFolder).Delete(true)        
        Process.Start("explorer.exe", sprintf "/select,\"%s\"" pathToErrEmlFile )

                

let write level format =
    Printf.kprintf (Help.write level ) format 

let info f = write Info f
let warn f = write Warn f
let error f = write Error f
let debug f = write Debug f
let save() = Help.save()

let saveEmailErrorReportToFile() = Help.saveEmailErrorReportToFile()