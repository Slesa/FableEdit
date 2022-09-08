[<RequireQualifiedAccess>]
module SetWaiterCall
open Api
open Feliz
open Feliz.Bulma
open Thoth.Json

type State = {
  Address: string
  Port: int
  Subject: string
  Message: string
}

type Msg =
  | SetAddress of string
  | SetPort of int
  | SetSubject of string
  | SetMessage of string
  | ChangeSettings of WaiterCallSettings


let init() =
  { Address = ""
    Port = 0
    Subject = ""
    Message = "" }


let update (msg: Msg) (state: State) =
  match msg with
  | SetAddress value ->
    { state with Address = value }
  | SetPort value ->
    { state with Port = value }
  | SetSubject value ->
    { state with Subject = value }
  | SetMessage value ->
    { state with Message = value }
  | ChangeSettings settings ->
    { state with Address = settings.Address; Port = settings.Port; Subject = settings.Subject; Message = settings.Message }


let render (state: State) (dispatch: Msg -> unit) =

  Bulma.field.div [
    Bulma.panel [
      Bulma.panelHeading [ prop.text "Waiter Call" ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "IP address of the server:"
          Bulma.control.div [
            Bulma.input.text [
              prop.valueOrDefault state.Address
              prop.onChange (SetAddress >> dispatch)
            ]
          ]
          // Bulma.help "Where does the Matrix POS put his job files"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Port:"
          Bulma.control.div [
            Bulma.input.number [
              prop.valueOrDefault state.Port
              prop.onChange (SetPort >> dispatch)
            ]
          ]
          //Bulma.help "Orders for which monitors should be ignored?"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Subject:"
          Bulma.control.div [
            Bulma.input.text [
              prop.valueOrDefault state.Subject
              prop.onChange (SetSubject >> dispatch)
            ]
          ]
          Bulma.help "Which is the subject to send in case of a waiter call?"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Message:"
          Bulma.control.div [
            Bulma.input.text [
              prop.valueOrDefault state.Message
              prop.onChange (SetMessage >> dispatch)
            ]
          ]
          Bulma.help "Which is the message to send in case of a waiter call?"
        ]
      ]
    ]
  ]


let decoder: Decoder<WaiterCallSettings> =
  Decode.object (fun get ->
    {
      Address = get.Required.Field "address" Decode.string
      Port = get.Required.Field "port" Decode.int
      Subject = get.Required.Field "subject" Decode.string
      Message = get.Required.Field "message" Decode.string
    })
