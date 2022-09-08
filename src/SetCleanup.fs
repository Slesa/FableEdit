[<RequireQualifiedAccess>]
module SetCleanup
open Api
open Feliz
open Feliz.Bulma
open Thoth.Json

type State = {
  StartAt: int * int
  XmlDays: int
  DbDays: int
  ArchiveDays: int
}

type Msg =
  | SetStartHour of int
  | SetStartMin of int
  | SetXmlDays of int
  | SetArchiveDays of int
  | SetDbDays of int
  | ChangeSettings of CleanSettings


let init() =
  { StartAt = ( 0, 0 )
    XmlDays = 0
    DbDays = 0
    ArchiveDays = 0 }


let update (msg: Msg) (state: State) =
  match msg with
  | SetStartHour hour ->
    { state with StartAt = (hour, snd state.StartAt) }
  | SetStartMin min ->
    { state with StartAt = (fst state.StartAt, min) }
  | SetXmlDays days ->
    { state with XmlDays = days }
  | SetArchiveDays days ->
    { state with ArchiveDays = days }
  | SetDbDays days ->
    { state with DbDays = days }
  | ChangeSettings settings ->
    { state with StartAt = settings.StartAt; XmlDays = settings.XmlDays; DbDays = settings.DbDays; ArchiveDays = settings.ArchiveDays }


let render (state: State) (dispatch: Msg -> unit) =
  
  Bulma.field.div [
    Bulma.panel [
      Bulma.panelHeading [ prop.text "Cleanup" ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Start at:"
          Bulma.field.div [
            Bulma.label "h"
            Bulma.input.number [
              prop.valueOrDefault (fst state.StartAt)
              prop.onChange (SetStartHour >> dispatch)
            ]
          ]
          Bulma.field.div [
            Bulma.label "m"
            Bulma.input.number [
              prop.valueOrDefault (snd state.StartAt)
              prop.onChange (SetStartMin >> dispatch)
            ]
          ]
          Bulma.control.div [
            DateTimePicker.dateTimePicker [
              //timePicker.clearLabel "Clear"
              //timePicker.onTimeSelected (fun (v:System.TimeSpan option) -> () ) // SetStartAt (v.Value.Hours, v.Value.Minutes) >> dispatch )
              //prop.valueOrDefault state.StartAt
              //prop.onChange (SetStartAt >> dispatch)
            ]
          ]
          Bulma.help "The time the cleanup task starts"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Archive older than days:"
          Bulma.control.div [
            Bulma.input.number [
              prop.valueOrDefault state.ArchiveDays
              prop.onChange (SetArchiveDays >> dispatch)
            ]
          ]
          Bulma.help "All database entries older than that will be set to archived, 0 turns off"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "XML days to keep:"
          Bulma.control.div [
            Bulma.input.number [
              prop.valueOrDefault state.XmlDays
              prop.onChange (SetXmlDays >> dispatch)
            ]
          ]
          Bulma.help "All job files older than that will be removed, 0 turns off"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Database days to keep:"
          Bulma.control.div [
            Bulma.input.number [
              prop.valueOrDefault state.DbDays
              prop.onChange (SetDbDays >> dispatch)
            ]
          ]
          Bulma.help "All database entries older than that will be removed, 0 turns off"
        ]
      ]
    ]
  ]

let decodeTime: Decoder<int * int> =
  Decode.object (fun get ->
    let hour = get.Required.Field "item1" Decode.int 
    let min = get.Required.Field "item2" Decode.int
    (hour, min))

let decoder: Decoder<CleanSettings> =
  Decode.object (fun get ->
    {
      StartAt = get.Required.Field "timeToCleanup" decodeTime
      XmlDays = get.Required.Field "daysToKeepXml" Decode.int
      DbDays = get.Required.Field "daysToKeepDb" Decode.int
      ArchiveDays = get.Required.Field "archiveDbOlderThanDays" Decode.int
    })

let decode json =
  Decode.fromString 
    (Decode.field "cleanup" decoder)
    json
