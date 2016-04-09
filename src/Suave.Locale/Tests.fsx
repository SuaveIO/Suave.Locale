#I "bin/Release"
#r "Suave.dll"
open Suave
#r "Arachne.Core.dll"
#r "Arachne.Language.dll"
#r "Arachne.Http.dll"
open Arachne.Http
open Arachne.Language
#r "Aether.dll"
#r "Chiron.dll"
open Chiron
#load "../../paket-files/haf/YoLo/YoLo.fs"
#load "Locale.fs"
open Suave.Locale

let (AcceptLanguage langs) = AcceptLanguage.parse("da, en-gb;q=0.8, en;q=0.7,*,lad-UK-Han")

(*

type IntlData with
  static member FromJson (_ : IntlData) =
    IntlData.create(Range ["en"])

and Messages with
  static member FromJson (_: Messages) =

*)
