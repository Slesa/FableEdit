[<RequireQualifiedAccess>]
module SetDatabase
open Api
open Feliz
open Feliz.Bulma
open Thoth.Json


type State = {
  Type: int
  Connection: string
}


type Msg =
  | SetType of int
  | SetConnection of string
  | ChangeSettings of DbSettings

let init() =
  { Type = 0
    Connection = "" }


let update (msg: Msg) (state: State) =
  match msg with
  | SetType value ->
    { state with Type = value }
  | SetConnection value ->
    { state with Connection = value }
  | ChangeSettings settings ->
    { state with Type = settings.Type; Connection = settings.Connection }


let render (state: State) (dispatch: Msg -> unit) =
  
  let typeValues = [
    0, "Microsoft SQL"
    1, "PostGRESQL"
    2, "SQLite"
  ]
  
  let dropValues =
    Bulma.select [
        //prop.onChange (fun v -> (SetType v.value) >> dispatch)
        prop.children [
            for (value, text) in typeValues do
                Html.option [
                    prop.value value
                    prop.text text ] ]
        prop.valueOrDefault state.Type
        prop.onChange (int >> SetType >> dispatch)
    ]

  Bulma.field.div [
    Bulma.panel [
      Bulma.panelHeading [ prop.text "Database" ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Type:"
          Bulma.control.div [
            dropValues
          ]
          Bulma.help "Which database do you use?"
        ]
      ]
      Bulma.panelBlock.div [
          match state.Type with
          | 0 
          | 1 -> // MSSql
            Bulma.field.div [
              Bulma.label "Server:"
              Bulma.input.text [
              ]
            ]
            Bulma.field.div [
              Bulma.label "User:"
              Bulma.input.text [
              ]
            ]
            Bulma.field.div [
              Bulma.label "Password:"
              Bulma.input.text [
              ]
            ]
            Bulma.field.div [
              Bulma.label "Database:"
              Bulma.input.text [
                prop.readOnly true
              ]
            ]
          | 2 ->
            Bulma.field.div [
              Bulma.label "File path:"
              Bulma.input.text [
              ]
            ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Connection:"
          Bulma.control.div [
            Bulma.input.text [
              prop.readOnly true
              prop.valueOrDefault state.Connection
              prop.onChange (SetConnection >> dispatch)
            ]
          ]
          Bulma.help "The connection string that is reported by your database"
        ]
      ]
    ]
  ]

let decoder: Decoder<DbSettings> =
  Decode.object (fun get ->
    {
      Type = get.Required.Field "type" Decode.int
      Connection = get.Required.Field "connection" Decode.string
    })

let decode json =
  Decode.fromString 
    (Decode.field "database" decoder)
    json
