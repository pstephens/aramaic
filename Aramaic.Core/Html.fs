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

namespace Aramaic.Core

open System.IO

module Html =

    type Part =
        | Doctype of string
        | Attribute of string * string
        | Element of string * List<Part>
        | LazyParts of (unit -> List<Part>)
        | Text of string

    type Document = List<Part>

    let doctype str = Doctype(str)
    let html content = Element("html", content)
    let body content = Element("body", content)
    let div content = Element("div", content)
    let span content = Element("span", content)
    let p content = Element("p", content)
    let text str = Text(str)
    let (:=) name v = Attribute(name, v)

    type RenderOptions = { indent: string; newline: string }
    let emptyRenderOptions = { indent = ""; newline = "" }
    let prettyRenderOptions = { indent = "\t"; newline = "\n" }


    // let renderContent

    // let renderAttributes

    let renderPart (wr: TextWriter) = function
    | Doctype(str) ->
        wr.Write("<!doctype ")
        wr.Write(str)
        wr.Write(">")
    | Element(name, content) ->
        wr.Write("<")
        wr.Write(name)
        wr.Write(">")
        wr.Write("</")
        wr.Write(name)
        wr.Write(">")
    | _ -> wr.Write("**unknown**")

    let render (opt: RenderOptions) (wr: TextWriter) (doc: Document) =
        doc |> List.iter (fun p -> renderPart wr p)
