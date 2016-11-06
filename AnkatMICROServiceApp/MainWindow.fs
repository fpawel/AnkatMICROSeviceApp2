module MainWindow

open System
open System.Windows.Forms
open System.Drawing



let aboutForm = 
    let x = new Widgets.AboutForm()        
    x.LabelVersion.Text <-  version |> Option.map string |> Option.getWith "-"
    x

let form =     
    let x = new Widgets.MainForm()
    
    let mutable isClosed = false

    x

[<AutoOpen>]
module private Helpers =

    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)

let setTooltip<'a when 'a :> Control > (x:'a) text = 
    tooltip.SetToolTip(x, text)



let errorMessageBox title message = 
    eprintfn "%A, %s" title message
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( message, title, MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore

let terminateApplication (e:Exception) = 
    eprintfn "Исключение %A" e 
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore
    Environment.Exit(1)

let initialize =
    let rec h = EventHandler(fun _ _ -> 
        aboutForm.Hide()
        aboutForm.FormBorderStyle <- FormBorderStyle.FixedDialog
        aboutForm.ControlBox <- true
        aboutForm.ShowInTaskbar <- false
        aboutForm.ShowIcon <- true
        form.Activated.RemoveHandler h)

    form.Activated.AddHandler h
    aboutForm.Show()
    aboutForm.Refresh()
    
    

    
    fun () -> ()