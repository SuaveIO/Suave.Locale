module Suave.Locale.Tests.Locale

open System
open Arachne.Http
open Arachne.Language
open Chiron
open Expecto
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Testing
open Suave.Locale
open Hopac

[<Tests>]
let range =
  testList "Range" [
    testList "generalise" [
      testCase "en-GB" <| fun _ ->
        let actual = Range.generalise (Range ["en"; "GB"])
        let expected = Range ["en"]
        Expect.equal actual expected "next up"

      testCase "en" <| fun _ ->
        let actual = Range.generalise (Range ["en"])
        let expected = Range ["en"]
        Expect.equal actual expected "next up"

      testCase "en-GB-XPrivate" <| fun _ ->
        let actual = Range.generalise (Range ["en"; "GB"; "XPrivate"])
        let expected = Range ["en"; "GB"]
        Expect.equal actual expected "next up"
      ]

    testList "checkParent" [
      testCase "en, en => Range [ en ]" <| fun _ ->
        let actual = Range.checkParent (Range ["en"]) (Range ["en"])
        let expected = Choice1Of2 (Range ["en"])
        Expect.equal actual expected ""

      testCase "en, en-GB => Choice1Of2 $ Range [ en ]" <| fun _ ->
        let actual = Range.checkParent (Range ["en"]) (Range ["en"; "GB"])
        let expected = Choice1Of2 (Range ["en"])
        Expect.equal actual expected ""

      testCase "en-GB, en => Choice2Of2 ()" <| fun _ ->
        let actual = Range.checkParent (Range ["en"; "GB"]) (Range ["en"])
        let expected = Choice2Of2 ()
        Expect.equal actual expected ""

      testCase "en, Any => Range [ en ]" <| fun _ ->
        let actual = Range.checkParent (Range ["en"]) (Range ["en"])
        let expected = Choice1Of2 (Range ["en"])
        Expect.equal actual expected ""
      ]
    ]

let emptyData (range : LanguageRange) =
  IntlData.create(
    range,
    """{"misc.title":"Häftig titel"}""" |> Json.parse |> Json.deserialize)

let wonkyMsgs = """{"locale":"en","messages":{"misc.title":"Awesome Title"}}"""

[<Tests>]
let intlData =
  testList "intl data" [
    testCase "from json" <| fun _ ->
      let intl : IntlData = Json.parse wonkyMsgs |> Json.deserialize
      let expected = Range ["en"]
      Expect.equal intl.locale expected "locale"

      let actual = intl |> IntlData.find "misc.title"
      Expect.equal actual "Awesome Title" "title"

    testCase "multi-length key" <| fun _ ->
      let data = {locale = Range ["en"]; messages = Map.empty |> Map.add "hello" "Hello World" }
      //printfn "json: %A" (data |> Json.serialize)
      let actual = data |> Json.serialize |> Json.deserialize
      Expect.equal actual data ""

    testCase "back and forth with single key-message" <| fun _ ->
      let data = {locale = Range ["en"]; messages = Map.empty |> Map.add "L^" "a" }
      let actual = data |> Json.serialize |> Json.deserialize
      Expect.equal actual data ""

    testPropertyWithConfig fsCheckConfig "back and forth" <| fun (intl : IntlData) ->
      // TODO: ensure that FsCheck doesn't generate identical translation keys k1, k2
      let subject : IntlData = intl |> Json.serialize |> Json.deserialize

      for KeyValue (k, tr) in intl.messages do
        try
          let actual = subject.messages |> Map.find k
          Expect.equal actual tr "eq"
        with e ->
          let b : string -> byte [] = System.Text.Encoding.UTF8.GetBytes
          printfn "key: %A, subject: %A, expected: %A" (b k) subject intl
          reraise ()

    testCase "merge" <| fun _ ->
      let svSE =
        [ "frontpage.menu.home", "Hem" // both have, this overwritten
          "frontpage.menu.logout", "Logga Ut" // fi doesn't have
          "settings.username", "haf" // fi doesn't have, new parent key
          "frontpage.menu.submenu", "TODO" // fi has extra level
        ] |> Map.ofList

      let svFI =
        [ "frontpage.menu.home", "Hemm" // both have, this chosedn
          "frontpage.menu.help", "Hjälp" // sv doesn't have
          "frontpage.menu.submenu.details", "Detaljer" // sv missing level, this chosen
        ] |> Map.ofList

      let expMerged =
        [ "frontpage.menu.home", "Hemm"
          "frontpage.menu.logout", "Logga Ut"
          "frontpage.menu.help", "Hjälp"
          "settings.username", "haf"
          "frontpage.menu.submenu.details", "Detaljer"
        ] |> Map.ofList

      let sv = IntlData.create(Range ["sv"; "SE"], svSE)
      let fi = IntlData.create(Range ["sv"; "FI"], svFI)

      let merged = IntlData.merge sv fi
      let expected = Range [ "sv"; "FI" ]
      Expect.equal merged.locale expected "merges locales to right locale"

      for KeyValue (k, tr) in expMerged do
        let actual = try merged |> IntlData.find k with _ -> ""
        Expect.equal actual tr (sprintf "merged %A" k)
    ]

[<Tests>]
let negotiate =
  let wants matching = function
    | Range rs as r when rs = matching ->
      //printfn "testing %A, matching %A" rs matching
      Choice1Of2 (emptyData r)
    | Any                              ->
      Choice1Of2 (emptyData (Range ["en"]))
    | rs                                ->
      //printfn "testing %A, not matching %A" rs matching
      Choice2Of2 ()
  let en = wants [ "en" ]
  let enGB = wants [ "en"; "GB" ]

  testList "Negotiate" [
    testList "findParent" [
      testCase "(Source 'en') Any => Choice1Of2 'en'" <| fun _ ->
        let actual = Negotiate.findParent en Any
        let expected = Choice1Of2 (emptyData (Range ["en"]))
        Expect.equal actual expected ""

      testCase "(Source 'en-GB') 'en' => Choice2Of2 ()" <| fun _ ->
        let actual = Negotiate.findParent enGB (Range ["en"])
        let expected = Choice2Of2 ()
        Expect.equal actual expected ""

      testCase "(Source 'en-GB') 'en-GB' => Choice2Of2 ()" <| fun _ ->
        let actual = Negotiate.findParent enGB (Range ["en"; "GB"])
        let expected = Choice1Of2 (emptyData (Range ["en"; "GB"]))
        Expect.equal actual expected ""

      testCase "(Source 'en') 'en-GB' => Choice1Of2 'en'" <| fun _ ->
        let actual = Negotiate.findParent en (Range ["en"; "GB"])
        let expected = Choice1Of2 (emptyData (Range ["en"]))
        Expect.equal actual expected ""
    ]
  ]

open HttpFs

[<Tests>]
let http =
  let neg =
    Negotiate.negotiate
      [ ReqSources.parseAcceptable
        ReqSources.always (Range [ "sv"; "FI" ])
      ]
      [ LangSources.fromJson wonkyMsgs
        LangSources.always (emptyData (Range ["sv"; "FI"])) ]
    |> Negotiate.assumeSource

  let clientNegf fHeader =
    let ctx = runWith defaultConfig (path "/i18n/messages" >=> Api.serveJson neg)

    try
      use resp =
        Client.Request.create Client.Get (Uri "http://127.0.0.1:8083/i18n/messages")
        |> fHeader
        |> Client.getResponse
        |> run

      let actual = resp.headers.[Client.ResponseHeader.Vary]
      let expected = "Accept-Encoding,Accept-Language"
      Expect.equal expected actual "Should have 'Vary: Accept-Encoding,Accept-Language'"

      let actual = resp.headers.[Client.ResponseHeader.ContentTypeResponse]
      let expected = "application/json; charset=utf-8"
      Expect.equal actual expected "Should have Content-Type: application/json; charset=utf-8"

      let actual = resp.headers.[Client.ResponseHeader.ContentLanguage]
      Expect.equal actual "sv-FI" "Should have Content-Language: sv-FI header"

      let data : IntlData =
        resp
        |> Client.Response.readBodyAsString
        |> run
        |> Json.parse
        |> Json.deserialize

      data

    finally
      disposeContext ctx

  let clientNeg header = clientNegf (Client.Request.setHeader (Client.RequestHeader.AcceptLanguage header))

  testList "Http" [
    testCase "negotiate" <| fun _ ->
      let data = clientNeg "ro, en, sv"
      let expected = Range [ "sv"; "FI" ]
      Expect.equal data.locale expected "locale"

    testCase "negotiate non existing, chooses default" <| fun _ ->
      let data = clientNeg "ro"
      let expected = Range [ "sv"; "FI" ]
      Expect.equal data.locale expected "locale"

    testCase "negotiate w/ mute client, choose default" <| fun _ ->
      let data = clientNegf id
      let expected = Range [ "sv"; "FI" ]
      Expect.equal data.locale expected "locale"
    ]