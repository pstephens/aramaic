(*

Copyright 2018 Peter Stephens

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace Aramaic.Core.Html

open Xunit
open FsUnit.Xunit
open FSharp.Markdown

module FromMarkdownTests =
    open Attribute

    let exerciseTransform doc = Aramaic.Core.Html.fromMarkdown doc

    [<Fact>]
    let ``Should render H1`` () =
        Markdown.Parse "# this is a heading"
        |> exerciseTransform
        |> should equal [ h1([], [ text("this is a heading") ]) ]

    [<Theory>]
    [<InlineData("#", "h1")>]
    [<InlineData("##", "h2")>]
    [<InlineData("###", "h3")>]
    [<InlineData("####", "h4")>]
    [<InlineData("#####", "h5")>]
    [<InlineData("######", "h5")>]
    let ``Should render Hn heading elements`` (headPrefix, expectedElement) =
        Markdown.Parse (headPrefix + "_the heading_")
        |> exerciseTransform
        |> should equal [ Element(expectedElement, [], [ em([], [ text("the heading") ]) ]) ]

    [<Fact>]
    let ``Should render Code block`` () =
        Markdown.Parse "```
this is a block of code
with a second line
```"
        |> exerciseTransform
        |> should equal [ pre([], [ code([], [ text("""this is a block of code
with a second line
""") ])])]

    [<Fact>]
    let ``Should render Code block with annotated language`` () =
        Markdown.Parse """```fsharp
let foo = 123
```"""
        |> exerciseTransform
        |> should equal [ pre([], [ code([classAttr:="lang-fsharp"], [ text("""let foo = 123
""") ])])]

    [<Fact>]
    let ``Should render unordered list`` () =
        """+ the first bullet
+ the second bullet"""
        |> Markdown.Parse
        |> exerciseTransform
        |> should equal
            [ ul([],
                [ li([], [ text("the first bullet") ])
                  li([], [ text("the second bullet") ])])]

    [<Fact>]
    let ``Should render nested unordered list`` () =
        """* the first bullet
    * sub-point a
    * sub-point b"""
        |> Markdown.Parse
        |> exerciseTransform
        |> should equal
            [ ul([],
                [ li([],
                    [ text("the first bullet")
                      ul([],
                        [ li([], [ text("sub-point a") ])
                          li([], [ text("sub-point b") ]) ]) ]) ]) ]

    [<Fact>]
    let ``Should render nested ordered list`` () =
        """1. the first bullet
    1. sub-point a
    2. sub-point b"""
        |> Markdown.Parse
        |> exerciseTransform
        |> should equal
            [ ol([],
                [ li([],
                    [ text("the first bullet")
                      ol([],
                        [ li([], [ text("sub-point a") ])
                          li([], [ text("sub-point b") ]) ]) ]) ]) ]

    [<Fact>]
    let ``Should render InlineCode`` () =
        Markdown.Parse "`this is some code`"
        |> exerciseTransform
        |> should equal [ p([], [ code([], [ text("this is some code") ]) ]) ]

    [<Fact>]
    let ``Should render Strong`` () =
        Markdown.Parse "**strong coffee**"
        |> exerciseTransform
        |> should equal [ p([], [ strong([], [ text("strong coffee") ]) ]) ]

    [<Fact>]
    let ``Should render Emphasis`` () =
        Markdown.Parse "_emphasized_ and also *emphasized*"
        |> exerciseTransform
        |> should equal [ p([], [ em([], [ text("emphasized") ])
                                  text(" and also ")
                                  em([], [ text("emphasized") ])
                                ])]

    [<Fact>]
    let ``Should render IndirectLink`` () =
        Markdown.Parse """[a][b]  [c]  [a **bad** link][d]

[b]: http://b.com
[c]: http://c.com "label"
        """
        |> exerciseTransform
        |> should equal
            [ p([], [ a([href:="http://b.com"], [ text("a") ])
                      text("  ")
                      a([href:="http://c.com"; title:="label"], [ text("c") ])
                      text("  ")
                      a([], [ text("a "); strong([], [text("bad")]); text(" link")])])]

    [<Fact>]
    let ``Should render AnchorLink`` () =
        MarkdownDocument([ Paragraph [ AnchorLink "foo" ] ], null)
        |> exerciseTransform
        |> should equal [ p([], [ a([name:="foo"], [ text("&#160;") ]) ] ) ]

    [<Fact>]
    let ``Should render DirectLink`` () =
        Markdown.Parse """[a](http://a.com/ "the title") [b](http://b.com/)"""
        |> exerciseTransform
        |> should equal
            [ p([], [ a([href:="http://a.com/"; title:="the title"], [ text("a") ])
                      text(" ")
                      a([href:="http://b.com/"], [ text("b") ]) ])]

    [<Fact>]
    let ``Should render IndirectImage`` () =
        Markdown.Parse """![alt1][a] ![alt2] [b]  ![alt3 ref not found] [c]

[a]: http://a.com/foo.jpg "title"
[b]: http://a.com/bar.jpg
        """
        |> exerciseTransform
        |> should equal
            [ p([], [ img([src:="http://a.com/foo.jpg"; alt:="alt1"; title:="title"])
                      text(" ")
                      img([src:="http://a.com/bar.jpg"; alt:="alt2"])
                      text("  ")
                      img([alt:="alt3 ref not found"])])]

    [<Fact>]
    let ``Should render DirectImage`` () =
        Markdown.Parse """![alt1](http://a.com/foo.jpg)  ![alt2](http://a.com/bar.jpg "with title")"""
        |> exerciseTransform
        |> should equal
            [ p([], [ img([src:="http://a.com/foo.jpg"; alt:="alt1"])
                      text("  ")
                      img([src:="http://a.com/bar.jpg"; alt:="alt2"; title:="with title"]) ])]

    [<Fact>]
    let ``Should render HardLineBreak`` () =
        Markdown.Parse "Line with two trailing spaces  \r\nThe next line"
        |> exerciseTransform
        |> should equal
            [ p([], [ text("Line with two trailing spaces")
                      br([])
                      text("The next line")])]

    [<Fact>]
    let ``Should render inline LaTex as span`` () =
        Markdown.Parse """Some LaTex $E=mc^2$"""
        |> exerciseTransform
        |> should equal
            [ p([], [ text("Some LaTex ")
                      span([classAttr:="inline-latex"], [ text("E=mc^2") ])])]

    [<Fact>]
    let ``Should render display LaTex as span`` () =
        Markdown.Parse """Some display mode LaTex $$E=mc^2$$"""
        |> exerciseTransform
        |> should equal
            [ p([], [ text("Some display mode LaTex ")
                      span([classAttr:="display-latex"], [ text("E=mc^2") ])])]

    [<Fact>]
    let ``Should render embedded spans`` () =
        MarkdownDocument(
            [ Paragraph [
                Literal("Some text")
                EmbedSpans { new MarkdownEmbedSpans with
                                member this.Render() =  [ Literal("more "); Literal("words ") ] }
                Literal("and the last words")]], null)
        |> exerciseTransform
        |> should equal
            [ p([], [ text("Some text")
                      text("more ")
                      text("words ")
                      text("and the last words") ])]

    [<Fact>]
    let ``Should render html literals`` () =
        Markdown.Parse """Some stuff

<b>bolded</b>

more stuff"""
        |> exerciseTransform
        |> should equal
            [ p([], [ text("Some stuff") ])
              HtmlLiteral("<b>bolded</b>")
              p([], [ text("more stuff")])]
