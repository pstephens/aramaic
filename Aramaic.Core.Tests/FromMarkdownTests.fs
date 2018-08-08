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

    [<Fact>]
    let ``Should render InlineCode`` () =
        Markdown.Parse "```this is some code```"
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