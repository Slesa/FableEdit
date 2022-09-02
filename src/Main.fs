module Main
open Elmish
open Elmish.React
  // @sforkmann  @zaid_ajaj @MangelMaxime @migueldeicaza @dsymetweets
  
Program.mkProgram App.init App.update App.render
  |> Program.withReactSynchronous "main"
  |> Program.run
