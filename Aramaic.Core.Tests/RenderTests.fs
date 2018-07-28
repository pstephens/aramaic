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

module RenderTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Aramaic.Core.Html

let exerciseRender model =
    use wr = new StringWriter()
    render emptyRenderOptions wr model
    wr.GetStringBuilder().ToString()

let exerciseRenderAttributes model =
    use wr = new StringWriter()
    renderAttributes wr model
    wr.GetStringBuilder().ToString()

[<Fact>]
let ``Should render doctype`` () =
    [ doctype "html" ] |> exerciseRender |> should equal "<!doctype html>"

[<Fact>]
let ``Basic element should render open and closing tags`` () =
    [ html ([], []) ] |> exerciseRender |> should equal "<html></html>"

[<Fact>]
let ``Element should render unquoted attributes`` () =
    [ body([bgcolor:="#FFF"], []) ]
    |> exerciseRender
    |> should equal "<body bgcolor=#FFF></body>"

[<Fact>]
let ``Void element should have no end tag`` () =
    [ br [] ] |> exerciseRender |> should equal "<br>"

[<Fact>]
let ``Raw text element should render start tag, end tag, and content`` () =
    [ script([], "if(foo > 5) { console.log('>5') }") ]
    |> exerciseRender
    |> should equal "<script>if(foo > 5) { console.log('>5') }</script>"

[<Fact>]
let ``RCData element should render start tag, end tag, and content`` () =
    [ title([], "Lions, Tigers, & Bears") ]
    |> exerciseRender
    |> should equal "<title>Lions, Tigers, & Bears</title>"

[<Fact>]
let ``renderAttributes should render regular attributes`` () =
    [ "foo" := "bar"; "a" := "b" ]
    |> exerciseRenderAttributes
    |> should equal """ foo=bar a=b"""

[<Fact>]
let ``renderAttributes should render empty attributes with missing value`` () =
    [ "foo" := ""; "a" := null; "nowrap" := null; "disabled" := "" ]
    |> exerciseRenderAttributes
    |> should equal """ foo="" a="" nowrap disabled"""
