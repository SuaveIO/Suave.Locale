[<AutoOpen>]
module Suave.Locale.Prelude

open System
open FsCheck
open Expecto
open Arachne.Language
open Suave.Locale

type Arbs =
  static member SafeString () =
    Arb.Default.String () |> Arb.filter (fun str -> not (String.IsNullOrWhiteSpace str) && not (Seq.exists Char.IsControl str))

  static member LanguageRange () =
    gen {
      let! b1 = ["en"; "sv"; "ch"] |> Gen.elements
      let! b2 = ["GB"; "FI"; "DK"] |> Gen.elements
      return Range [b1; b2]
    }
    |> Arb.fromGen

  static member MessageKey () =
    Gen.nonEmptyListOf (Arb.generate<string>) |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Arbs> ] }
    