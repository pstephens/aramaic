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

    type Attribute =
        | Attribute of string * string

    type Part =
        | Doctype of string
        | Element of string * List<Attribute> * List<Part>
        | VoidElement of string * List<Attribute>
        | RawTextElement of string * List<Attribute> * string
        | RCData of string * List<Attribute> * string
        | Text of string

    type Document = List<Part>

    let doctype str = Doctype(str)
    let html (attr, content) = Element("html", attr, content)

    let area attr = VoidElement("area", attr)
    let base' attr = VoidElement("base", attr)
    let body (attr, content) = Element("body", attr, content)
    let br attr = VoidElement("br", attr)
    let col attr = VoidElement("col", attr)
    let command attr = VoidElement("command", attr)
    let div (attr, content) = Element("div", attr, content)
    let embed attr = VoidElement("embed", attr)
    let hr attr = VoidElement("hr", attr)
    let img attr = VoidElement("img", attr)
    let input attr = VoidElement("input", attr)
    let keygen attr = VoidElement("keygen", attr)
    let link attr = VoidElement("link", attr)
    let meta attr = VoidElement("meta", attr)
    let p (attr, content) = Element("p", attr, content)
    let param attr = VoidElement("param", attr)
    let script (attr, str) = RawTextElement("script", attr, str)
    let source attr = VoidElement("source", attr)
    let span (attr, content) = Element("span", attr, content)
    let style (attr, str) = RawTextElement("style", attr, str)
    let title (attr, str) = RCData("title", attr, str)
    let textarea (attr, str) = RCData("textarea", attr, str)
    let track attr = VoidElement("track", attr)
    let wbr attr = VoidElement("wbr", attr)

    let text str = Text(str)
    let (:=) name value = Attribute(name, value)

    let bgcolor = "bgcolor"

    type RenderOptions = { indent: string; newline: string }
    let emptyRenderOptions = { indent = ""; newline = "" }
    let prettyRenderOptions = { indent = "\t"; newline = "\n" }


    // let renderContent

    let rec renderAttributes (wr: TextWriter) = function
    | Attribute(name, value)::tail ->
        wr.Write(" ");
        wr.Write(name);
        wr.Write("=");
        wr.Write(value);
        renderAttributes wr tail
    | [] -> ()

    let renderPart (wr: TextWriter) = function
    | Doctype(str) ->
        wr.Write("<!doctype ")
        wr.Write(str)
        wr.Write(">")
    | Element(name, attr, content) ->
        wr.Write("<")
        wr.Write(name)
        renderAttributes wr attr
        wr.Write(">")
        wr.Write("</")
        wr.Write(name)
        wr.Write(">")
    | VoidElement(name, attr) ->
        wr.Write("<")
        wr.Write(name)
        wr.Write(">")
    | RawTextElement(name, attr, content)
    | RCData(name, attr, content) ->
        wr.Write("<")
        wr.Write(name)
        wr.Write(">");
        wr.Write(content);
        wr.Write("</");
        wr.Write(name);
        wr.Write(">");
    | _ ->
        wr.Write("**unknown**")

    let render (opt: RenderOptions) (wr: TextWriter) (doc: Document) =
        doc |> List.iter (fun p -> renderPart wr p)
