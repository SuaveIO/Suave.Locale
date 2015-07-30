# Suave Locale

An internationalisation WebPart and library for Suave for use with
[React-Intl](https://github.com/yahoo/react-intl) and [Format.JS](http://formatjs.io/)
or other i18n systems.

This library's only purpose; to deliver:

``` javascript
var intlData = {
  locales : ['en-GB'],
  messages: {
    post: {
      meta: 'Posted {ago}, {num, plural, one{# comment} other{# comments}}'
    }
  }
};
```

Which you can then use with your i18n infrastructure, like such:

``` javascript
<FormattedMessage
    message={this.getIntlMessage('post.meta')}
    num={this.props.post.comments.length}
    ago={<FormattedRelative value={this.props.post.date} />} />
```

## Usage (HTTP)

This part is Suave-specific.There are two moving parts; the negotiation and the
localised contents. The library is built around these functions:

``` fsharp
open Suave.Locale

type ReqSource = HttpRequest -> Choice<AcceptLanguage, unit>
type IntlSource = LanguageRange -> Choice<IntlData, unit>

type TryLangNeg = ReqSource list -> IntlSource list
                  -> HttpRequest -> Choice<IntlData, unit>
type LangNeg    =    HttpRequest -> IntlData
```

`ReqSource` takes a `Suave.Types.HttpRequest` and maybe returns a *range* of
accepted languages. The most commonly used request source is
`ReqSources.parseAcceptable` which parses `Accept-Language` HTTP-header into a
range.

`IntlSource` takes a language range and maybe returns localised contents for
this range. You should have one of these that returns the default site language
last in the list: `IntlSource list`, and others for each supported language.

`TryLangNeg` is the type of `Negotiate.negotiate`; it's the actual language
negotiation function. You can turn it into a `LangNeg` by doing

``` fsharp
let neg : LangNeg =
  Negotiate.negotiate
    [ ReqSources.parseQs "locale"
      ReqSources.parseCookie "locale"
      ReqSources.parseAcceptable
      ReqSources.always (Range [ "en" ])]
    [ LangSources.testAndGetJson test get
      LangSources.always (defaultLanguage appCtx.settings.rootPath) ]
  |> Negotiate.assumeSource
```

This first tries to find a query string, `locale`, then the cookie `locale`,
then the header `Acceptable-Language` and if all of these fail, the `en` locale.
`Negotiate.assumeSource` assumes we get a Choice1Of2 from **both** the
`ReqSources` and the `LangSources`.

Now you can feed the `LangNeg` into the combinator:

``` fsharp
let app : WebPart =
  choose [
    Suave.Locale.Http.api "/intl" neg
    Browse.filesHome
    ]
```

And you have yourself a splendid new internationalisation system!