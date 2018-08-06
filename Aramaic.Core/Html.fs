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

        let inline (:=) name value = Attribute(name, value)

        let bgcolor = "bgcolor"
        let href = "href"
        let name = "name"
        let style = "style"
        let title = "title"

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

    open Attribute


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

    let inline a (attr, content) = Element("a", attr, content)
    let inline area attr = VoidElement("area", attr)
    let inline baseEl attr = VoidElement("base", attr)
    let inline body (attr, content) = Element("body", attr, content)
    let inline br attr = VoidElement("br", attr)
    let inline col attr = VoidElement("col", attr)
    let inline command attr = VoidElement("command", attr)
    let inline code (attr, content) = Element("code", attr, content)
    let inline div (attr, content) = Element("div", attr, content)
    let inline em (attr, content) = Element("em", attr, content)
    let inline embed attr = VoidElement("embed", attr)
    let inline h1 (attr, content) = Element("h1", attr, content)
    let inline h2 (attr, content) = Element("h2", attr, content)
    let inline h3 (attr, content) = Element("h3", attr, content)
    let inline h4 (attr, content) = Element("h4", attr, content)
    let inline h5 (attr, content) = Element("h5", attr, content)
    let inline hr attr = VoidElement("hr", attr)
    let inline img attr = VoidElement("img", attr)
    let inline input attr = VoidElement("input", attr)
    let inline keygen attr = VoidElement("keygen", attr)
    let inline link attr = VoidElement("link", attr)
    let inline meta attr = VoidElement("meta", attr)
    let inline p (attr, content) = Element("p", attr, content)
    let inline param attr = VoidElement("param", attr)
    let inline script (attr, str) = RawTextElement("script", attr, str)
    let inline source attr = VoidElement("source", attr)
    let inline span (attr, content) = Element("span", attr, content)
    let inline strong (attr, content) = Element("strong", attr, content)
    let inline styleEl (attr, str) = RawTextElement("style", attr, str)
    let inline titleEl (attr, str) = RCData("title", attr, str)
    let inline textarea (attr, str) = RCData("textarea", attr, str)
    let inline track attr = VoidElement("track", attr)
    let inline wbr attr = VoidElement("wbr", attr)

    let inline text str = Text(str)

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
        doc |> List.iter (renderPart wr)

    type IDictionary<'a, 'b> = System.Collections.Generic.IDictionary<'a, 'b>

    type MarkdownCtx =
        { definedLinks : IDictionary<string, string * option<string>>; }

    let rec fromSpan (ctx : MarkdownCtx) (x: MarkdownSpan) : Part =
        match x with
        | Literal(t) -> text(t)
        | InlineCode(content) -> code([], [ text(content) ])
        | Strong(content) -> strong([], fromSpans ctx content)
        | Emphasis(content) -> em([], fromSpans ctx content)
        | IndirectLink(content, _, FSharp.Markdown.Html.LookupKey ctx.definedLinks (link, title'))
        | DirectLink(content, (link, title')) ->
            let titleAttr : List<Attribute> =
                title'
                |> Option.map(fun t -> [ "title":=t ])
                |> Option.defaultValue []
            a( (href:=link) :: titleAttr, fromSpans ctx content)
        | IndirectLink(content, _, _) ->
            a([], fromSpans ctx content)
        | AnchorLink(y) -> a([name:=y], [])

    and fromSpans (ctx : MarkdownCtx) (spans : MarkdownSpans) : List<Part> =
        spans |> List.map (fromSpan ctx)

    let fromParagraph (ctx : MarkdownCtx) (par : MarkdownParagraph) : Part =
        match par with
        | Heading(1, spans) -> h1([], fromSpans ctx spans)
        | Paragraph(spans) -> p([], fromSpans ctx spans)

    let fromMarkdown (doc : MarkdownDocument) : Document =
        let ctx = { definedLinks = doc.DefinedLinks }
        doc.Paragraphs |> List.map (fromParagraph ctx)
