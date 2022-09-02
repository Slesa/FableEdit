[<RequireQualifiedAccess>]
module Index
open Api
open Deferred
open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp

type State = {
  Version: DeferredResult<string>
}

type Msg =
| GetVersion of AsyncOperationStatus<Result<string, string>>


let init() =
  let state = { Version = HasNotStartedYet }
  let cmd = Cmd.ofMsg (GetVersion Started)
  state, cmd

let startLoading (state: State) =
  { state with Version = InProgress }


let loadBmVersion: Async<Msg> = async {
    let endpoint = bmEndpoint Functions.Version
    let! status, response = Http.get endpoint
      //let! response = Http.request endpoint
      //|> Http.method GET
      //|> Http.content (BodyContent.Text "{ }")
      //|> Http.header (Headers.contentType "application/json")
      //|> Http.header (Headers.create "Access-Control-Allow-Origin" "*")
      //|> Http.send
    match status with
    | 200 ->
        return GetVersion(Finished (Ok response))
        (*let storyIds = Decode.fromString (Decode.list Decode.int) responseText
        match storyIds with
        | Ok storyIds ->
            let storyItems = List.truncate 10 storyIds
            return LoadStoryItems (Finished (Ok storyItems))        
        | Error errorMsg -> 
            return LoadStoryItems (Finished (Error errorMsg))
        *)
    | _ -> 
        return GetVersion (Finished (Error response))
}


let update (msg: Msg) (state: State) : (State * Cmd<Msg>) =
  match msg with

  | GetVersion Started ->
      let nextState = { startLoading state with Version = InProgress }
      let nextCmd = Cmd.fromAsync (loadBmVersion)
      nextState, nextCmd

  | GetVersion (Finished (Ok version)) -> 
      let nextState = { state with Version = Resolved(Ok version) }
      nextState, Cmd.none

  | GetVersion (Finished (Error error)) -> 
      let nextState = { state with Version = Resolved(Error error) }
      nextState, Cmd.none



let render (state: State) (dispatch: Msg -> unit) =
  match state.Version with
  | HasNotStartedYet -> 
      Html.h3 [
        prop.style [ style.color.yellow ]
        prop.text "BonMonitor Server not yet reached"
      ]
  | InProgress -> //spinner
      Html.h3 [
        prop.style [ style.color.yellow ]
        prop.text "Calling BonMonitor Server..."
      ]
  | Resolved (Error error) -> // renderError error
      Html.h3 [
        prop.style [ style.color.red ]
        prop.text (sprintf "BonMonitor Server reports error %s" error)
      ]
  | Resolved (Ok version) -> 
      Html.h3 [
        prop.style [ style.color.green ]
        prop.text (sprintf "BonMonitor Server version %s" version)
      ]

