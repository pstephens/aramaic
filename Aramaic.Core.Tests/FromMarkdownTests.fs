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
