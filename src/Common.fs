[<AutoOpen>]
module Common
open Feliz
open Elmish


module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd


let renderSettingsGroup (title: string) =
  Html.h2 [
    prop.className "title"
    prop.text title
  ] 


let renderFormField (label: string) (input: ReactElement) =
  Html.div [
    Html.div label
    input
  ]

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]


let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]