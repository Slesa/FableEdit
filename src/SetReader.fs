[<RequireQualifiedAccess>]
module SetReader
open Api
open Feliz
open Feliz.Bulma
open Thoth.Json

type State = {
  InputPath: string
  Excludes: string
  ArticleText: string
}

type Msg =
  | SetInputPath of string
  | SetExcludes of string
  | SetArticleText of string
  | ChangeSettings of ReaderSettings


let init() =
  { InputPath = ""
    Excludes = ""
    ArticleText = "" }


let update (msg: Msg) (state: State) =
  match msg with
  | SetInputPath value ->
    { state with InputPath = value }
  | SetExcludes value ->
    { state with Excludes = value }
  | SetArticleText value ->
    { state with ArticleText = value }
  | ChangeSettings settings ->
    { state with InputPath = settings.InputPath; Excludes = settings.Excludes; ArticleText = settings.ArticleText }


let render (state: State) (dispatch: Msg -> unit) =

  let typeValues = [
    "", "Article text"
    "short", "Article's short text"
    "print", "Article's printer text"
    "display", "Article's display text"
  ]
  
  let dropValues =
    Bulma.select [
        //prop.onChange (fun v -> (SetType v.value) >> dispatch)
        prop.children [
            for (value, text) in typeValues do
                Html.option [
                    prop.value value
                    prop.text text ] ]
        prop.valueOrDefault state.ArticleText
        prop.onChange (SetArticleText >> dispatch)
    ]

  Bulma.field.div [
    Bulma.panel [
      Bulma.panelHeading [ prop.text "Import" ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Read POS files from:"
          Bulma.control.div [
            Bulma.input.text [
              prop.valueOrDefault state.InputPath
              prop.onChange (SetInputPath >> dispatch)
            ]
          ]
          Bulma.help "Where does the Matrix POS put his job files"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Excludes:"
          Bulma.control.div [
            Bulma.input.text [
              prop.valueOrDefault state.Excludes
              prop.onChange (SetExcludes >> dispatch)
            ]
          ]
          Bulma.help "Orders for which monitors should be ignored?"
        ]
      ]
      Bulma.panelBlock.div [
        Bulma.field.div [
          Bulma.label "Article text:"
          Bulma.control.div [
            dropValues
          ]
          Bulma.help "Which article value should be imported as article text"
        ]
      ]
    ]
  ]

let decoder: Decoder<ReaderSettings> =
  Decode.object (fun get ->
    {
      InputPath = ""
      Excludes = ""
      //InputPath = get.Required.Field "spoolPaths" Decode.string
      //Excludes = get.Required.Field "excludes" Decode.string
      ArticleText = get.Required.Field "articleTextField" Decode.string
    })
