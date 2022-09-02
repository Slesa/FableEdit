[<RequireQualifiedAccess>]
module App
open Elmish
open Feliz
open Feliz.Bulma

type Page =
  | Index
  | Settings
  | Clients

let allPages: Page list = [
  Page.Index
  Page.Settings
  Page.Clients
]

let pageName = function
  | Page.Index -> "Home"
  | Page.Settings -> "Settings"
  | Page.Clients -> "Monitors"


type State = {
  Index: Index.State
  Settings: Settings.State
  Clients: Clients.State
  CurrentPage: Page
}

type Msg =
  | IndexMsg of Index.Msg
  | SettingsMsg of Settings.Msg
  | ClientsMsg of Clients.Msg
  | SwitchPage of Page


let init() =
  let indexState, indexCmd = Index.init()
  let cmdIndex = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) indexCmd
  let settingsState, settingsCmd = Settings.init()
  let cmdSettings = Cmd.map (fun settingsMsg -> Msg.SettingsMsg settingsMsg) settingsCmd
  let initialState = {
    Index = indexState
    Settings = settingsState
    Clients = Clients.init()
    CurrentPage = Page.Index
  }
  initialState, Cmd.batch [cmdSettings; cmdIndex]


let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
  match msg with

  | SwitchPage page ->
      let nextState = { state with CurrentPage = page }
      nextState, Cmd.none

  | IndexMsg indexMsg -> 
      let nextState, nextCmd = Index.update indexMsg state.Index
      let appCmd = Cmd.map (fun indexMsg -> Msg.IndexMsg indexMsg) nextCmd
      { state with Index = nextState }, appCmd

  | SettingsMsg settingsMsg ->
      //let nextState, nextCmd = Settings.update settingsMsg state.Settings
      let nextState, nextCmd = Settings.update settingsMsg state.Settings
      let appCmd = Cmd.map (fun indexMsg -> Msg.SettingsMsg indexMsg) nextCmd
      { state with Settings = nextState }, appCmd

  | ClientsMsg clientsMsg ->
      let nextState, nextCmd = Clients.update clientsMsg state.Clients
      { state with Clients = nextState }, nextCmd


let renderTitle (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20; style.marginBottom 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "BonMonitor Server"
    ]
    ]
  ]

let renderTabs state dispatch =
  let switchPage page =
    if state.CurrentPage <> page
    then dispatch (SwitchPage page)

  Bulma.tabs [
    tabs.isBoxed 
    tabs.isFullWidth
  
  //Html.div [
  //  prop.className [ "tabs"; "is-toggle"; "is-fullwidth" ]
    prop.children [
      Html.ul [
        for page in allPages ->
        Bulma.tab [
          Html.li [
            prop.classes [ if state.CurrentPage = page then "is-active" ]
            prop.onClick (fun _ -> switchPage page)
            prop.children [
              Html.a [ Html.span (pageName page) ]
            ]
          ]
        ]
      ]
    ]
  ]

let renderPage state dispatch =
  match state.CurrentPage with
  | Page.Index ->
    Index.render state.Index (IndexMsg >> dispatch)
  | Page.Settings ->
    Settings.render state.Settings (SettingsMsg >> dispatch)
  | Page.Clients ->
    Clients.render state.Clients (ClientsMsg >> dispatch)



let render (state: State) (dispatch: Msg -> unit) =
  
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      renderTitle state dispatch
      renderTabs state dispatch
      renderPage state dispatch
    ]
  ]