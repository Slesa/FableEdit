//[<AutoOpen>]
module Api
open Fable.SimpleHttp



[<RequireQualifiedAccess>]
type Functions =
  | Version
  | GetSettings
  | PutSettings
  | GetClients

type DbSettings = {
  Type: int
  Connection: string
}

type CleanSettings = {
  StartAt: int * int
  XmlDays: int
  DbDays: int
  ArchiveDays: int
}

type ReaderSettings = {
  InputPath: string
  Excludes: string
  ArticleText: string
}

type WaiterCallSettings = {
  Address: string
  Port: int
  Subject: string
  Message: string
}

type BmSettings = {
  Database: DbSettings
  Cleanup: CleanSettings
  Reader: ReaderSettings
  WaiterCall: WaiterCallSettings
}


let bmEndpoint func =
  let fromBaseUrl =  sprintf "http://localhost:8282/bm/%s"
  match func with
  | Functions.Version -> fromBaseUrl "version"
  | Functions.GetSettings -> fromBaseUrl "settings"

 