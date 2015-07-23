# Suave Locale

An internationalisation WebPart and library for Suave for use with [React-Intl](https://github.com/yahoo/react-intl) or other i18n systems.

This library's only purpose; to deliver:

``` json
var intlData = {
    locales : ['en-US'],
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
`ReqSources.parseAcceptable` which parses `Accept-Language` into a range.

`IntlSource` takes a language range and maybe returns localised contents for
this range. You should have one of these that returns the default site language
last in the list: `IntlSource list`, and others for each supported language.

`TryLangNeg` is the type of `Negotiate.negotiate`; it's the actual language
negotiation function. You can turn it into a `LangNeg` by doing

``` fsharp
Negotiate.negotiate [ .. ] [ .. ] |> Negotiate.assumeSource
// => (it : LangNeg)
```