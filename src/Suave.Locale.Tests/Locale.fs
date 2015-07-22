module Suave.Locale.Tests

open System
open Suave
open Suave.Web
open Suave.Http
open Suave.Types
open Suave.Testing
open Arachne.Http
open Arachne.Language
open Chiron
open Suave.Locale
open Fuchu

[<Tests>]
let range =
  testList "Range" [
    testList "generalise" [
      testCase "en-GB" <| fun _ ->
        Assert.Equal("next up", Range ["en"], Range.generalise (Range ["en"; "GB"]))
      testCase "en" <| fun _ ->
        Assert.Equal("next up", Range ["en"], Range.generalise (Range ["en"]))
      testCase "en-GB-XPrivate" <| fun _ ->
        Assert.Equal("next up", Range ["en"; "GB"], Range.generalise (Range ["en"; "GB"; "XPrivate"]))
      ]
    testList "checkParent" [
      testCase "en, en => Range [ en ]" <| fun _ ->
        Assert.Equal("", Choice1Of2 (Range ["en"]), Range.checkParent (Range ["en"]) (Range ["en"]))
      testCase "en, en-GB => Choice1Of2 $ Range [ en ]" <| fun _ ->
        Assert.Equal("", Choice1Of2 (Range ["en"]), Range.checkParent (Range ["en"]) (Range ["en"; "GB"]))
      testCase "en-GB, en => Choice2Of2 ()" <| fun _ ->
        Assert.Equal("", Choice2Of2 (), Range.checkParent (Range ["en"; "GB"]) (Range ["en"]))
      testCase "en, Any => Range [ en ]" <| fun _ ->
        Assert.Equal("", Choice1Of2 (Range ["en"]), Range.checkParent (Range ["en"]) (Range ["en"]))
      ]
    ]

let emptyData r =
  IntlData.Create(
    [r],
    """{"misc":{"title":"Häftig titel"}}""" |> Json.parse |> Json.deserialize)

let wonkyData = """{"locales":["sv-SE"],"messages":{"misc":{"title":"Awesome Title"}}}"""

[<Tests>]
let intlData =
  testList "intl data" [
    testCase "from json" <| fun _ ->
      let rec find k (Messages kvs) = List.find (fun (key, value) -> key = k) kvs |> snd
      let intl : IntlData = Json.parse wonkyData |> Json.deserialize
      Assert.Equal("locale", [ Range ["sv"; "SE"] ], intl.locales)
      Assert.Equal("title", "Awesome Title", find ["misc";"title"] intl.messages)
    ]

[<Tests>]
let negotiate =
  let createSource matching = function
    | Range rs as r when rs = matching -> Choice1Of2 (emptyData r)
    | Any                              -> Choice1Of2 (emptyData (Range ["en"]))
    | _                                -> Choice2Of2 ()
  let en = createSource [ "en" ]
  let enGB = createSource [ "en"; "GB" ]

  testList "Negotiate" [
    testList "findParent" [
      testCase "(Source 'en') Any => Choice1Of2 'en'" <| fun _ ->
        Assert.Equal("", Choice1Of2 (emptyData (Range ["en"])), Negotiate.findParent en Any)
      testCase "(Source 'en-GB') 'en' => Choice2Of2 ()" <| fun _ ->
        Assert.Equal("", Choice2Of2 (), Negotiate.findParent enGB (Range ["en"]))
      testCase "(Source 'en-GB') 'en-GB' => Choice2Of2 ()" <| fun _ ->
        Assert.Equal("",
                     Choice1Of2 (emptyData (Range ["en"; "GB"])),
                     Negotiate.findParent enGB (Range ["en"; "GB"]))
    ]
  ]

open HttpFs

[<Tests>]
let http =
  let neg =
    Negotiate.negotiate
      [ ReqSources.parseAcceptable ]
      [ LangSources.fromJson wonkyData
        function _ -> Choice1Of2 (emptyData (Range ["sv"; "SE"]))
      ]
    |> Negotiate.assumeSource


  let clientNeg header =
    let ctx = runWith defaultConfig (Http.app "/intl" neg)
    try
      use resp =
        Client.createRequest Client.Get (Uri "http://127.0.0.1:8083/intl")
        |> Client.withHeader (Client.RequestHeader.AcceptLanguage "ro")
        |> Client.getResponse
        |> Async.RunSynchronously
      
      let data : IntlData =
        resp
        |> Client.Response.readBodyAsString
        |> Async.RunSynchronously
        |> Json.parse
        |> Json.deserialize

      data

    finally
      disposeContext ctx

  testList "Http" [
    testCase "negotiate" <| fun _ ->
      let data = clientNeg "ro, en, sv"
      Assert.Equal("locales", [ Range [ "sv"; "SE" ] ], data.locales)
    ]