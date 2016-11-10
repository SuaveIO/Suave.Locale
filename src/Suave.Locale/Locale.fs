/// Serves the the app messages or negotiates language in your app, calling you
/// back with the response.
namespace Suave.Locale
#nowarn "64"

open Suave
open Suave.Locale.YoLo
open Arachne.Http
open Arachne.Language
open Chiron

module internal List =
  let tryPickCh f def =
    let rec inner = function
      | [] ->
        Choice2Of2 def

      | x :: xs ->
        match f x with
        | Choice1Of2 x ->
          Choice1Of2 x

        | Choice2Of2 _ ->
          inner xs

    inner

module Range =
  /// Find
  let generalise = function
    | Any ->
      Any

    | Range [] ->
      Range []

    | Range [c] ->
      Range [c]

    | Range cs ->
      Range (List.rev (List.tail (List.rev cs)))

  /// Find the matching child sub-range for the given parent
  let checkParent parent mchild =
    match parent, mchild with
    | Any, _ ->
      Choice1Of2 mchild

    | _, Any ->
      Choice2Of2 ()

    | Range ps, Range cs ->
      let rec find acc = function
        | [] ->
          Choice2Of2 ()

        | lang :: xs ->
          let acc' = acc @ [ lang ]
          if ps = acc' then Choice1Of2 acc' else find acc' xs

      find [] cs |> Choice.map Range

  let isParent parent mchild =
    match checkParent parent mchild with
    | Choice1Of2 _ ->
      true

    | _ ->
      false

  let toString = function
    | Any ->
      "*/*"

    | Range rs ->
      String.concat "-" rs

type MessageKey = string
type Translation = string
type Messages = Map<MessageKey, Translation>

open Chiron.Operators

type IntlData =
  { locale   : LanguageRange
    messages : Messages }

  static member create (range : LanguageRange, messages : Messages) =
    { locale   = range
      messages = messages }

  static member FromJson (_ : _) : Json<_> =
    (fun ls m -> { locale = LanguageRange.parse ls; messages = m })
    <!> Json.read "locale"
    <*> Json.read "messages"

  static member ToJson (x : IntlData) =
    Json.write "locale" (Range.toString x.locale)
    *> Json.write "messages" x.messages

  static member ToJson msgs : Json<unit> =
    Json.Optic.set Aether.Optics.id_ (Object (msgs |> Map.map (fun k v -> String v)))

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module IntlData =
  /// Find the key in the list of translations
  let rec find k intl =
    intl.messages |> Map.find k

  /// Merge the translations from b into a, overwriting exiting items of a and
  /// in the case of differing hierarchies of the two translations, choses the
  /// hierarchy from b.
  let merge (a : IntlData) (b : IntlData) =
    let combined = b.messages |> Map.fold (fun acc key t -> acc |> Map.put key t) a.messages
    { locale   = b.locale
      messages = combined }

/// Return IntlData if you have data for the given range; always return your data
/// if you're given the Any range. For non-Any ranges, only return for exact
/// range match.
type IntlSource = LanguageRange -> Choice<IntlData, unit>

module LangSources =

  /// Always serve this translation if it's queried. Even if the negotiated
  /// locale is something different.
  let always intl : IntlSource =
    fun _ -> Choice1Of2 intl

  /// Serve this `intl` translation only if the 'matching' LanguageRange
  /// parameter matches the negotiated. Example:
  ///
  /// LangSources.onlyIf (Range [ "en" ]) intl
  let forRange matching intl : IntlSource =
    fun actual -> if matching = actual then Choice1Of2 intl else Choice2Of2 ()

  /// Create an IntlSource from the json string. The string needs to be possible
  /// to deserialise into an IntlData structure.
  let fromJson jsonStr : IntlSource =
    let data = Json.parse jsonStr |> Json.deserialize
    fun range ->
      if data.locale = range || range = Any then
        Choice1Of2 data
      else
        Choice2Of2 ()

  // en.json
  // en-GB.json
  // testAndGet exists deser ["en"; "US"] => Choice2Of2 ()
  // testAndGet . . ["en"] => Choice1Of2 ( ... )
  // testAndGet . . ["en"; "GB"] => Choice1Of2 ( ... )

  // en.json
  // testAndGet exists deser ["en"; "US"] => Choice2Of2 ()
  // testAndGet . . ["en"] => Choice1Of2 ( ... )
  // testAndGet . . ["en"; "GB"] => Choice2Of2 ()

  /// given Range["en"; "SE"; "Private"] tests "en-SE-Private.json".
  /// given Any tests "_.json"
  let testAndGetJson test get : IntlSource =
    fun range ->
      let name =
        match range with
        | Any ->
          "_.json"

        | Range ns ->
          (String.concat "-" ns) + ".json"

      if test name then Choice1Of2 (get name) else Choice2Of2 ()

  open Chiron
  open System.IO

  //let jsonFile =
  //  testAndGetJson File.Exists (File.ReadAllText >> Json.parse >> Json.deserialize)

type ReqSource = HttpRequest -> Choice<AcceptLanguage, (* error msg *) string>

module ReqSources =

  /// Parse the locale from the Accept-Language header
  let parseAcceptable : ReqSource =
    fun req ->
      req.header "accept-language"
      |> Choice.bind (AcceptLanguage.tryParse)

  /// Find the locale from a cookie with a name
  let parseCookie name : ReqSource =
    fun req ->
      req.header "cookie"
      |> Choice.map Cookie.parseCookies
      |> Choice.bind (
        List.tryFind (fun (cookie : HttpCookie) -> String.equalsCaseInsensitive name cookie.name)
        >> Choice.ofOption (sprintf "Cookie named '%s' not found in request" name)
        >> Choice.bind ((fun c -> c.value) >> AcceptLanguage.tryParse))

  /// Find the locale from the query string
  let parseQuery name : ReqSource =
    fun req ->
      req.queryParam name
      |> Choice.bind (AcceptLanguage.tryParse)

  let always range : ReqSource =
    fun _ ->
      Choice1Of2 (AcceptLanguage [ AcceptableLanguage (range, None) ])

type TryLangNeg =
  ReqSource list -> IntlSource list -> HttpRequest -> Choice<IntlData, unit>

type LangNeg =
  HttpRequest -> IntlData

module Negotiate =

  (* Do a depth-first search of the accepted langs, sources and parent-keys
     of the given language range.
  *)

  // findParent s ["en";"GB"]
  //   s ["en"; "GB"] => Choice2Of2 ()
  //   s ["en"] => Choice1Of2 x
  // => Choice2Of2 x
  // findParent s ["en";"GB"]
  //   s ["en"; "GB"; "GGG"] => Choice1Of2 x
  //   s ["en"; "GB"] => Choice2Of2 ()
  //   s ["en"] => Choice2Of2 ()
  // => Choice2Of2 x

  let findParent (source : IntlSource) target =
    let rec inner curr =
      match source curr with
      | Choice1Of2 x ->
        Choice1Of2 x

      | Choice2Of2 () ->
        let next = Range.generalise curr
        //printfn "next=%A, curr=%A, eq=%b" next curr (next = curr)
        if next = curr then Choice2Of2 ()
        else inner next

    inner target

  let findSource (sources : IntlSource list) (AcceptableLanguage (r, w)) =
    let rec inner = function
      | [] ->
        Choice2Of2 ()

      | s :: ss ->
        match findParent s r with
        | Choice1Of2 x -> Choice1Of2 x
        | Choice2Of2 _ -> inner ss

    inner sources

  let findIntl sources (AcceptLanguage langs) =
    let rec inner = function
      | [] ->
        Choice2Of2 ()

      | l :: ls ->
        match findSource sources l with
        | Choice1Of2 x ->
          Choice1Of2 x

        | Choice2Of2 _ ->
          inner ls

    inner langs

  let negotiate : TryLangNeg =
    fun matchers sources ->
      fun req ->
        matchers
        |> List.tryPickCh (fun m -> m req) ()
        |> Choice.bind (findIntl sources)

  /// You can do:
  ///   Negotiate.negotiate [ ... ] [ ... ] |> Negotiate.assumeSource
  /// to get back a:
  ///   LangNeg
  ///
  let assumeSource f =
    fun x ->
      f x
      |> function
      | Choice1Of2 x ->
        x

      | Choice2Of2 () ->
        failwithf "some language source didn't return a value properly for %A, like assumed it would" x

  let negotiateDefault sources : LangNeg =
    let defaults =
      [ ReqSources.parseAcceptable
        ReqSources.parseCookie "locale" ]
      // ReqSources.parseQs "locale" // not a good default as it may be shared in links

    fun req ->
      match negotiate defaults sources req with
      | Choice1Of2 x ->
        x

      | Choice2Of2 _ ->
        failwithf "no IntlSource returned a translation, of %A" sources

module Api =
  open Suave
  open Suave.Operators
  open Suave.Successful
  open Suave.Filters
  open Suave.Writers
  open Negotiate

  /// Sets header values on the Vary header, such as:
  /// 'Vary: Accept-Encoding,Accept-Language' using the `setHeaderValue`
  /// function.
  let setVary : WebPart=
    // see https://www.fastly.com/blog/best-practices-for-using-the-vary-header#comment-1751365055
    setHeaderValue "Vary" "Accept-Encoding"
    >=> setHeaderValue "Vary" "Accept-Language"

  /// Sets the Content-Language header value to what has been negotiated.
  let setContentLanguage (data : IntlData) : WebPart =
    setHeaderValue "Content-Language" (Range.toString data.locale)

  let setHeaders (data : IntlData) : WebPart =
    setVary >=> setContentLanguage data

  /// Runs the negotiator and feeds the result into `fCont`. Doesn't cause any
  /// side-effects to happen to the response
  let negotiate (negotiator : LangNeg) (fCont : IntlData -> WebPart) : WebPart =
    request (negotiator >> fCont)

  /// Runs the negotiator, assumes `fCont` changes its output to the negotiated
  /// language so it sets Content-Language, and then passes the request to
  /// fCont.
  let negotiateWithHeaders (negotiator : LangNeg) (fCont : IntlData -> WebPart) : WebPart =
    negotiate negotiator (fun data ->
      setContentLanguage data
      >=> setVary
      >=> fCont data)

  /// Directly serve the IntlData as a JSON structure; doesn't enforce GET or
  /// path.
  let serveJson (negotiate : LangNeg) : WebPart =

    let serveData (data : IntlData) : WebPart =
      warbler (fun _ -> data |> Json.serialize |> Json.format |> OK)
      >=> setMimeType "application/json; charset=utf-8"

    negotiateWithHeaders negotiate serveData