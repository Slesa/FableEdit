[<RequireQualifiedAccess>]
module Settings
open Deferred
open Api
open Elmish
open Elmish.React
open Feliz
open Feliz.Bulma
open Fable.SimpleHttp
open Thoth.Json


type State = {
  Database: SetDatabase.State
  Cleanup: SetCleanup.State
  Reader: SetReader.State
  WaiterCall: SetWaiterCall.State
  Settings: DeferredResult<BmSettings>
}

type Msg =
  | SetDatabase of SetDatabase.Msg
  | SetCleanup of SetCleanup.Msg
  | SetReader of SetReader.Msg
  | SetWaiterCall of SetWaiterCall.Msg
  | LoadSettings of AsyncOperationStatus<Result<BmSettings, string>>


let init() = 
  let state = { 
    Database = SetDatabase.init()
    Cleanup = SetCleanup.init()
    Reader = SetReader.init()
    WaiterCall = SetWaiterCall.init()
    Settings = HasNotStartedYet
  }
  let cmd = Cmd.ofMsg (LoadSettings Started)
  state, cmd


let startLoading (state: State) =
  { state with Settings = InProgress }


let decoder : Decoder<BmSettings> =
  Decode.object ( fun get -> {
    Database = get.Required.Field "database" SetDatabase.decoder
    Cleanup = get.Required.Field "cleanup" SetCleanup.decoder
    Reader = get.Required.Field "reader" SetReader.decoder
    WaiterCall = get.Required.Field "waiterCall" SetWaiterCall.decoder
  })

let decodeSettings json =
  Decode.fromString decoder json

let loadSettings (state: State): Async<Msg> = async {
  let endpoint = bmEndpoint Functions.GetSettings
  let! status, response = Http.get endpoint
  match status with
    | 200 ->
        match (decodeSettings response) with
        | Ok settings -> 
          return LoadSettings (Finished (Ok settings))
        | Error error -> 
          return LoadSettings (Finished (Error error))
    | _ -> 
      return LoadSettings (Finished (Error response))
}


let update (msg: Msg) (state: State) =

  match msg with

  | SetDatabase msg -> 
      let nextState = SetDatabase.update msg state.Database
      //let appCmd = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) nextCmd
      { state with Database = nextState }, Cmd.none //, appCmd

  | SetCleanup msg -> 
      let nextState = SetCleanup.update msg state.Cleanup
      //let appCmd = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) nextCmd
      { state with Cleanup = nextState }, Cmd.none //, appCmd

  | SetReader msg -> 
      let nextState = SetReader.update msg state.Reader
      //let appCmd = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) nextCmd
      { state with Reader = nextState }, Cmd.none //, appCmd

  | SetWaiterCall msg -> 
      let nextState = SetWaiterCall.update msg state.WaiterCall
      //let appCmd = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) nextCmd
      { state with WaiterCall = nextState }, Cmd.none //, appCmd

  | LoadSettings Started ->
      let nextState = { startLoading state with Settings = InProgress }
      let nextCmd = Cmd.fromAsync (loadSettings state)
      nextState, nextCmd

  | LoadSettings (Finished (Ok settings)) -> 
      let nextState = { state with Settings = Resolved(Ok settings) }
      let dbCmd = Cmd.ofMsg( SetDatabase.ChangeSettings settings.Database )
      let dbApp = Cmd.map (fun dbMsg -> Msg.SetDatabase dbMsg) dbCmd
      let cleanCmd = Cmd.ofMsg( SetCleanup.ChangeSettings settings.Cleanup )
      let cleanApp = Cmd.map (fun cleanMsg -> Msg.SetCleanup cleanMsg) cleanCmd
      let readerCmd = Cmd.ofMsg( SetReader.ChangeSettings settings.Reader )
      let readerApp = Cmd.map (fun readerMsg -> Msg.SetReader readerMsg) readerCmd
      let wcallCmd = Cmd.ofMsg( SetWaiterCall.ChangeSettings settings.WaiterCall )
      let wcallApp = Cmd.map (fun wcallMsg -> Msg.SetWaiterCall wcallMsg) wcallCmd
      nextState, Cmd.batch [cleanApp; dbApp; readerApp; wcallApp]

  | LoadSettings (Finished (Error error)) -> 
      let nextState = { state with Settings = Resolved(Error error) }
      nextState, Cmd.none


let render (state: State) (dispatch: Msg -> unit) =
  Html.form [
    prop.style [ style.padding 20 ]
    prop.children [
      match state.Settings with
      | HasNotStartedYet ->  //Html.none
          Html.h3 [
            prop.style [ style.color.yellow ]
            prop.text "BonMonitor Server not yet reached"
          ]
      | InProgress -> // spinner
          Html.h3 [
            prop.style [ style.color.yellow ]
            prop.text "Calling BonMonitor Server..."
          ]
      | Resolved (Error error) -> //renderError error
          Html.h3 [
            prop.style [ style.color.red ]
            prop.text (sprintf "BonMonitor Server reports error %s" error)
          ]
      | Resolved (Ok settings) -> 
          (*Html.h3 [
            prop.style [ style.color.green ]
            prop.text (sprintf "BonMonitor Server settings")
          ]*)
          SetDatabase.render state.Database (SetDatabase >> dispatch)
          SetCleanup.render state.Cleanup (SetCleanup >> dispatch)
          SetReader.render state.Reader (SetReader >> dispatch)
          SetWaiterCall.render state.WaiterCall (SetWaiterCall >> dispatch)
      Bulma.field.div [
        field.isGrouped
        field.isGroupedCentered
        prop.children [
          Bulma.control.div [
            Bulma.button.button [
              Bulma.color.isLink
              prop.text "Submit"
            ]
          ]
        ]
      ]
    ]
  ]
