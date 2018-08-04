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
open FSharp.Markdown

module Html =
    open System

    module Attribute =
        type Attribute =
            | Attribute of string * string

        let (:=) name value = Attribute(name, value)

        let bgcolor = "bgcolor"
        let href = "href"
        let style = "style"

        let private booleanAttributes =
                set [ "async"; "autocomplete"; "autofocus"; "autoplay"; "border"; "challenge"; "checked";
                      "compact";"contenteditable"; "controls"; "default"; "defer"; "disabled"; "formNoValidate";
                      "frameborder"; "hidden"; "indeterminate"; "ismap"; "loop"; "multiple"; "muted"; "nohref";
                      "noresize"; "noshade"; "novalidate"; "nowrap"; "open"; "readonly"; "required"; "reversed";
                      "scoped"; "scrolling"; "seamless"; "selected"; "sortable"; "spellcheck"; "translate" ]

        [<Flags>]
        type private CharClass =
            None = 0
            | EmptyString = 1
            | Quot = 2
            | Apos = 4
            | Quotable = 8
            | EveryNonEmptyClass = 14

        let rec private classify (str : string, i, acc) =
            if i >= str.Length then
                if str.Length = 0 then
                    CharClass.EmptyString
                else
                    acc
            else
                let ch = str.[i]
                if ch = '"' then
                    classify(str, i + 1, acc ||| CharClass.Quot)
                else if ch = '\'' then
                    classify(str, i + 1, acc ||| CharClass.Apos)
                else if ch = '<' || ch = '>' || ch = '`' || ch = ' ' || ch = '=' then
                    classify(str, i + 1, acc ||| CharClass.Quotable)
                else
                    classify(str, i + 1, acc)

        let rec renderAttributes (wr: TextWriter) = function
        | Attribute(name, value)::tail ->
            wr.Write(" ")
            wr.Write(name)
            let cls = classify(value, 0, CharClass.None)
            if cls = CharClass.EmptyString then
                if not (Set.contains name booleanAttributes) then
                    wr.Write("=\"\"")
            else
                wr.Write("=")
                if (cls &&& CharClass.Quot) <> CharClass.None then
                    if(cls &&& CharClass.Apos) <> CharClass.None then
                        wr.Write("\"")
                        wr.Write(value.Replace("\"", "&quot;"))
                        wr.Write("\"")
                    else
                        wr.Write("'")
                        wr.Write(value)
                        wr.Write("'")
                else if (cls &&& (CharClass.Apos ||| CharClass.Quotable)) <> CharClass.None then
                    wr.Write("\"")
                    wr.Write(value)
                    wr.Write("\"")
                else
                    wr.Write(value)

            renderAttributes wr tail
        | [] -> ()

    type Part =
        | Doctype of string
        | Element of string * List<Attribute.Attribute> * List<Part>
        | VoidElement of string * List<Attribute.Attribute>
        | RawTextElement of string * List<Attribute.Attribute> * string
        | RCData of string * List<Attribute.Attribute> * string
        | Text of string

    type Document = List<Part>


    let doctype str = Doctype(str)
    let html (attr, content) = Element("html", attr, content)

    let area attr = VoidElement("area", attr)
    let baseEl attr = VoidElement("base", attr)
    let body (attr, content) = Element("body", attr, content)
    let br attr = VoidElement("br", attr)
    let col attr = VoidElement("col", attr)
    let command attr = VoidElement("command", attr)
    let code (attr, content) = Element("code", attr, content)
    let div (attr, content) = Element("div", attr, content)
    let em (attr, content) = Element("em", attr, content)
    let embed attr = VoidElement("embed", attr)
    let h1 (attr, content) = Element("h1", attr, content)
    let h2 (attr, content) = Element("h2", attr, content)
    let h3 (attr, content) = Element("h3", attr, content)
    let h4 (attr, content) = Element("h4", attr, content)
    let h5 (attr, content) = Element("h5", attr, content)
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
    let strong (attr, content) = Element("strong", attr, content)
    let styleEl (attr, str) = RawTextElement("style", attr, str)
    let title (attr, str) = RCData("title", attr, str)
    let textarea (attr, str) = RCData("textarea", attr, str)
    let track attr = VoidElement("track", attr)
    let wbr attr = VoidElement("wbr", attr)

    let text str = Text(str)

    type RenderOptions = { indent: string; newline: string }
    let emptyRenderOptions = { indent = ""; newline = "" }
    let prettyRenderOptions = { indent = "\t"; newline = "\n" }

    let rec private renderContent (wr: TextWriter) = function
    | part :: tail ->
        renderPart wr part
        renderContent wr tail
    | [] -> ()

    and renderPart (wr: TextWriter) = function
    | Doctype(str) ->
        wr.Write("<!doctype ")
        wr.Write(str)
        wr.Write(">")
    | Element(name, attr, content) ->
        wr.Write("<")
        wr.Write(name)
        Attribute.renderAttributes wr attr
        wr.Write(">")
        renderContent wr content
        wr.Write("</")
        wr.Write(name)
        wr.Write(">")
    | VoidElement(name, attr) ->
        wr.Write("<")
        wr.Write(name)
        Attribute.renderAttributes wr attr
        wr.Write(">")
    | RawTextElement(name, attr, content)
    | RCData(name, attr, content) ->
        wr.Write("<")
        wr.Write(name)
        Attribute.renderAttributes wr attr
        wr.Write(">")
        wr.Write(content)
        wr.Write("</")
        wr.Write(name)
        wr.Write(">")
    | Text(str) -> wr.Write(str)

    let render (opt: RenderOptions) (wr: TextWriter) (doc: Document) =
        doc |> List.iter (fun p -> renderPart wr p)

    let rec fromSpan (span: MarkdownSpan) : Part =
        match span with
        | Literal(t) -> text(t)
        | InlineCode(content) -> code([], [ text(content) ])
        | Strong(content) -> strong([], fromSpans(content))
        | Emphasis(content) -> em([], fromSpans(content))

    and fromSpans (spans : MarkdownSpans) : List<Part> =
        spans |> List.map fromSpan

    let fromParagraph (par : MarkdownParagraph) : Part =
        match par with
        | Heading(1, spans) -> h1([], fromSpans spans)
        | Paragraph(spans) -> p([], fromSpans spans)

    let fromMarkdown (doc : MarkdownDocument) : Document =
        doc.Paragraphs |> List.map fromParagraph
