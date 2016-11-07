module MyWinForms.Converters

open System
open System.ComponentModel
open System.Drawing.Design
open System.Windows.Forms
open System.Windows.Forms.Design
open System.Globalization

type Header() = 
    inherit TypeConverter()
    override x.CanConvertTo(_, destType) =  
        destType = typeof<string>
    override x.ConvertTo(_, _, _, _)  =
        box "<...>"

type TypeConverter with
    static member standardValuesCollection x =
        new TypeConverter.StandardValuesCollection(x)

   
type BooleanTypeConverter(true':string, false':string) = 
    inherit BooleanConverter()
    
    override __.ConvertTo(_,_,value,_) =
        if value |> box :?> bool then true' else false'
        |> box
    
    override __.ConvertFrom(_,_,value) =
        value |> box :?> string = true' 
        |> box

type YesNoConverter() =
    inherit BooleanTypeConverter("Да", "Нет")


