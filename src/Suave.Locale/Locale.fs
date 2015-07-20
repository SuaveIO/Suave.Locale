module Suave.Locale

open Suave.Types
open Arachne.Http
open Arachne.Language
open Chiron

[<AutoOpen>]
module internal Prelude =
  module Choice =
    let map f = function
      | Choice1Of2 x -> Choice1Of2 (f x)
      | Choice2Of2 y -> Choice2Of2 y

    let mapSnd f = function
      | Choice1Of2 x -> Choice1Of2 x
      | Choice2Of2 y -> Choice2Of2 (f y)


    let bind f = function
      | Choice1Of2 x -> f x
      | Choice2Of2 y -> Choice2Of2 y

    let bindSnd f = function
      | Choice1Of2 x -> Choice1Of2 x
      | Choice2Of2 y -> f y

    let force def = function
      | Choice1Of2 x -> x
      | Choice2Of2 _ -> def

    let ofOption def = function
      | Some x -> Choice1Of2 x
      | None   -> Choice2Of2 def

  module Option =
    let ofChoice = function
      | Choice1Of2 x -> Some x
      | Choice2Of2 _ -> None

  module List =
    let tryPickCh f def =
      let rec inner = function
        | [] -> Choice2Of2 def
        | x :: xs ->
          match f x with
          | Choice1Of2 x -> Choice1Of2 x
          | Choice2Of2 _ -> inner xs
      inner

module Range =
  /// Find
  let generalise = function
    | Any       -> Any
    | Range []  -> Range []
    | Range [c] -> Range [c]
    | Range cs  -> Range (List.rev (List.tail (List.rev cs)))

  /// Find the matching child sub-range for the given parent
  let checkParent parent mchild =
    match parent, mchild with
    | Any, _ -> Choice1Of2 mchild
    | _, Any -> Choice2Of2 ()
    | Range ps, Range cs ->
      let rec find acc = function
        | [] -> Choice2Of2 ()
        | lang :: xs ->
          let acc' = acc @ [ lang ]
          if ps = acc' then Choice1Of2 acc' else find acc' xs
      find [] cs |> Choice.map Range

  let isParent parent mchild =
    match checkParent parent mchild with
    | Choice1Of2 _ -> true
    | _ -> false

type MessageKey = string list
type Translation = string

open Chiron.Operators

type IntlData =
  { locale   : LanguageRange
    messages : Messages }

  static member Create (range : LanguageRange, ?messages : Messages) =
    { locale   = range
      messages = defaultArg messages (Messages []) }

  static member FromJson (_ : _) : Json<_> =
    (fun l m -> { locale = LanguageRange.Parse l; messages = m })
    <!> Json.read "locale"
    <*> Json.read "messages"

and Messages = Messages of (MessageKey * Translation) list

with
  static member FromJson (_ : Messages) : Json<Messages> =
    // { "k": { .. }, "k2": ".." }
    let rec parse (key : MessageKey) = function
      | Json.String translation ->
        [ key, translation ]
      | Json.Object map ->
        Map.fold (fun s k t ->
          let key' : MessageKey = key @ [k]
          parse key' t @ s) [] map
      | Json.Array arr ->
        List.concat (List.map (parse key) arr)
      | _ ->
        []
    fun json ->
      Value (Messages (parse [] json)), json

/// Return IntlData if you have data for the given range; always return your data
/// if you're given the Any range. For non-Any ranges, only return for exact
/// range match.
type IntlSource = LanguageRange -> Choice<IntlData, unit>

module LangSources =

  let always intl : IntlSource =
    fun _ -> Choice1Of2 intl

  let fromJson jsonStr : IntlSource =
    let data = Json.parse jsonStr |> Json.deserialize
    fun range ->
      if range = data.locale || range = Any then Choice1Of2 data
      else Choice2Of2 ()

  // en.json
  // en-GB.json
  // testAndGet exists deser ["en"; "US"] => Choice2Of2 ()
  // testAndGet . . ["en"] => Choice1Of2 ( ... )
  // testAndGet . . ["en"; "GB"] => Choice1Of2 ( ... )

  // en.json
  // testAndGet exists deser ["en"; "US"] => Choice2Of2 ()
  // testAndGet . . ["en"] => Choice1Of2 ( ... )
  // testAndGet . . ["en"; "GB"] => Choice2Of2 ()

  /// given Range["en"; "SE"; "Private"] tests "en_SE_Private.json".
  /// given Any tests "_.json"
  let testAndGetJson test get : IntlSource =
    fun range ->
      let name =
        match range with
        | Any -> "_.json"
        | Range ns -> (String.concat "_" ns) + ".json"
      if test name then Choice1Of2 (get name) else Choice2Of2 ()

  open Chiron
  open System.IO

  //let jsonFile =
  //  testAndGetJson File.Exists (File.ReadAllText >> Json.parse >> Json.deserialize)

module Negotiate =

  type ReqSource = HttpRequest -> Choice<AcceptLanguage, unit>

  module ReqSources =

    /// Parse the locale from the Accept-Language header
    let parseAcceptable : ReqSource =
      fun req ->
        req.header "accept-language"
        |> Choice.mapSnd (fun _ -> ())
        |> Choice.bind (fun str -> AcceptLanguage.TryParse(str) |> Choice.ofOption ())

    /// Find the locale from a cookie with a name
    let parseCookie name : ReqSource =
      fun req ->
        Choice2Of2 ()

    /// Find the locale from the query string
    let parseQs name : ReqSource =
      fun req ->
        req.queryParam name
        |> Choice.mapSnd (fun _ -> ())
        |> Choice.bind (fun str -> AcceptLanguage.TryParse(str) |> Choice.ofOption ())

  (* Do a depth-first search of the accepted langs, sources and parent-keys
     of the given language range.
  *)

  type TryLangNeg = ReqSource list -> IntlSource list -> HttpRequest -> Choice<IntlData, unit>
  type LangNeg = HttpRequest -> IntlData

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
      | Choice1Of2 x -> Choice1Of2 x
      | Choice2Of2 () ->
        let next = Range.generalise curr
        if next = curr then Choice2Of2 ()
        elif not (Range.isParent target next) then Choice2Of2 ()
        else inner next
    inner target

  let findSource (sources : IntlSource list) (AcceptableLanguage (r, w)) =
    let rec inner = function
      | [] -> Choice2Of2 ()
      | s :: ss -> match findParent s r with
                   | Choice1Of2 x -> Choice1Of2 x
                   | Choice2Of2 _ -> inner ss
    inner sources

  let findIntl sources (AcceptLanguage langs) =
    let rec inner = function
      | [] -> Choice2Of2 ()
      | l :: ls -> match findSource sources l with
                   | Choice1Of2 x -> Choice1Of2 x
                   | Choice2Of2 _ -> inner ls
    inner langs

  let negotiate : TryLangNeg =
    fun matchers sources ->
      fun req ->
        matchers
        |> List.tryPickCh (fun m -> m req) ()
        |> Choice.bind (findIntl sources)

  let negotiateDefault sources : LangNeg =
    let defaults =
      [ ReqSources.parseAcceptable
        ReqSources.parseQs "lang"
        ReqSources.parseCookie "lang" ]
    fun req ->
      match negotiate defaults sources req with
      | Choice1Of2 x -> x
      | Choice2Of2 _ -> failwithf "no IntlSource returned a translation, of %A" sources

/// Serves the localisation files
module Http =
  open Suave
  open Suave.Types
  open Suave.Http
  open Suave.Http.Applicatives
  open Suave.Http.Writers
  open Negotiate

  let app matchPath (negotiate : LangNeg) : WebPart =
    GET
    >>= path matchPath
    >>= request (negotiate >> Json.serialize >> Json.format)
    >>= setMimeType "application/json"