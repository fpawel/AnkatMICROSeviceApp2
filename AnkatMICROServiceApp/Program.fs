open System
open System.IO
open System.Windows.Forms
open System.Threading
open System.Diagnostics

let onException ( e : exn ) =
    Trace.error "%A" e
    Trace.save()
    let x = new Widgets.FormErrorMessageDialog()    
    
    for form in [ for x in  Application.OpenForms -> x] do
        if not <| obj.ReferenceEquals(x,form) then
            form.Close()
    let _ = x.ShowDialog()
    let _ = Trace.saveEmailErrorReportToFile()
    ()
    

let main () = 
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault true
    MainWindow.initialize()        
    Application.Run MainWindow.form
    Trace.save()
    

let onAnotherInstanceExist() = 
    MessageBox.Show
        (   "Нельзя создать более одного экземпляра приложения" ,
            "Анкат-7664МИКРО", 
            MessageBoxButtons.OK, 
            MessageBoxIcon.Information ) 
    |> ignore   
    

let mutexid = "Global\\{59174D5B-2B8F-4A2A-AE7C-479D537A1680}"

open System.Security.Principal
open System.Security.AccessControl

[<EntryPoint>]
[<STAThread>]
do
    
    use mutex = new System.Threading.Mutex(false, mutexid)
    let si = new SecurityIdentifier(WellKnownSidType.WorldSid, null)

    let allowEveryoneRule =             
        new MutexAccessRule(si, MutexRights.FullControl, AccessControlType.Allow)   
    let securitySettings = new MutexSecurity();
    securitySettings.AddAccessRule(allowEveryoneRule);
    mutex.SetAccessControl(securitySettings)
        
    try
        let hasHandle = mutex.WaitOne(0, false);
        if not hasHandle then onAnotherInstanceExist() else
        main()
        failwith "ddd"
        
    with
    | :? AbandonedMutexException ->
        onAnotherInstanceExist()
    | e ->
        onException e