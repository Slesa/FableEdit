[<RequireQualifiedAccess>]
module Clients
open Elmish
open Elmish.React
open Feliz


type State = {
    Input: string
}


type Msg = 
  | AddClient
  | EditClient
  | CopyClient



let init() = 
  { Input = "" }


let update (msg: Msg) (state: State) =
  state, Cmd.none

  
let render (state: State) (dispatch: Msg -> unit) =
  Html.h3 "Clients active"
