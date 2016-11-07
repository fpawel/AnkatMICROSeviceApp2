module AppConfig

open System
open System.IO

open System
open System.Text.RegularExpressions

type ApplicatioConfig = 
    {   SimonTiming     : Comport.Timing
        ModbusTiming    : Comport.Timing
        PortName        : string  }

    static member create() = {   
        SimonTiming     = Comport.Timing.dummy()
        ModbusTiming    = Comport.Timing.dummy()
        PortName        = ""  }

let get, set = Json.Config.create "app.config.json" ApplicatioConfig.create

