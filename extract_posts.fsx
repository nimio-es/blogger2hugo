#!/bin/env -S dotnet fsi

#r "nuget: HtmlAgilityPack, 1.11.37"

module backup =

    open System.IO
    open System.Text
    open System.Xml
    open System.Xml.Serialization

    [<CLIMutable>]
    [<XmlTypeAttribute("category")>]
    type Category = {
        [<XmlAttribute("scheme")>]
        Scheme: string
        [<XmlAttribute("term")>]
        Term: string
    }

    [<CLIMutable>]
    [<XmlTypeAttribute("value")>]
    type TypeValue = {
        [<XmlAttribute("type")>]
        Type : string
        [<XmlText>]
        Value: string
    }

    [<CLIMutable>]
    [<XmlTypeAttribute("link")>]
    type Link = {
        [<XmlAttribute("rel")>]
        Rel: string

        [<XmlAttribute("type")>]
        Type: string

        [<XmlAttribute("href")>]
        Href: string

        [<XmlAttribute("title")>]
        Title: string
    }

    [<CLIMutable>]
    [<XmlTypeAttribute("entry")>]
    type Entry = {
        [<XmlElement("id")>]
        Id: string

        [<XmlElement("category")>]
        Categories: Category array

        [<XmlElement("published")>]
        Published: string

        [<XmlElement("updated")>]
        Updated: string

        [<XmlElement("title")>]
        Title: TypeValue

        [<XmlElement(ElementName = "content", IsNullable = true)>]
        Content: TypeValue

        [<XmlElement("link")>]
        Links: Link array
    }

    [<CLIMutable>]
    [<XmlRoot(Namespace = "http://www.w3.org/2005/Atom", ElementName = "feed")>]
    type Feed = {
        [<XmlElement("id")>]
        Id: string

        [<XmlElement("updated")>]
        Updated: string

        [<XmlElement("title")>]
        Title: TypeValue

        [<XmlElement("link")>]
        Links: Link array

        [<XmlElement("entry")>]
        Entries: Entry array
    }

    let read (path: string) =
        let serializer = XmlSerializer(typeof<Feed>)
        use reader = new StreamReader(path, Encoding.UTF8)
        let xmlText = reader.ReadToEnd()
        use mem = new MemoryStream(Encoding.UTF8.GetBytes xmlText)
        serializer.Deserialize mem :?> Feed

module utils =
    let slugging (s:string) =
        s.ToLower().Replace("http://","").Replace("https://","").Replace(".com.es",".com").Replace("/","-").Replace(".","_").Replace("%20","-")

    let aliased (s:string) =
        s.Replace("http://","").Replace("https://","").Replace(".com.es",".com").Replace("unomascero.blogspot.com","").Replace("%20","-")


module hugo =

    module model =
        type matter = {
            id: string
            draft: bool
            date: string
            title: string
            description: string
            slug: string
            tags: string list
            categories: string list
            externalLink: string
            series: string list
            aliases: string list
        }

        type post = {
            frontMatter: matter
            content: string
        }

    type storablePost = {
        fileName: string
        post: model.post
    }

    let private postsDatabase = System.Collections.Generic.Dictionary<string, storablePost>()

    let private proposeFileName (originalTitle: string) (originalDate: string) =
        let mutable title = originalTitle.ToLower()
        title <- title.Replace(" ", "-")
        title <- title.Replace("á","a")
        title <- title.Replace("é","e")
        title <- title.Replace("í","i")
        title <- title.Replace("ó","o")
        title <- title.Replace("ú","u")
        title <- title.Replace("ñ","n")
        title <- title.Replace(",", "_")
        title <- title.Replace(".", "_")
        title <- title.Replace(":", "_")
        title <- title.Replace(";", "_")
        title <- title.Replace("(", "")
        title <- title.Replace(")", "")
        title <- title.Replace("¡", "")
        title <- title.Replace("!", "")
        title <- title.Replace("¿", "")
        title <- title.Replace("?", "")
        title <- title.Replace("'", "")
        title <- title.Replace("´", "")
        title <- title.Replace("\"","")
        title <- title.Replace("_-","-")
        title <- title.Replace("--","-")
        title <- title.Replace("__","_")
        title <- title.Replace("_-","-")
        title <- title.Replace("--","-")
        title <- title.Replace("__","_")
        title <- title.Replace("_-","-")
        title <- title.Replace("--","-")
        title <- title.Replace("__","_")
        title <- title.Replace("<","")
        title <- title.Replace(">","")
        title <- title.Replace("@","")
        title <- title.Replace("\\","")
        title <- title.Replace("/","")
        title <- title.Replace("&","")
        title <- title.Replace("*","")
        title <- title.Replace("%","")
        title <- title.Replace("#","")
        title <- title.Replace("%20","-")
        title <- title.Replace("«","")
        title <- title.Replace("»","")
        title <- title.Replace("…", "")
        let date = originalDate.Replace("-", "").Replace("T", "").Replace(":", "").Substring(0,12)
        sprintf "%s_%s.md" date title

    let store (post: model.post): storablePost =
        let post' = {
            fileName = proposeFileName post.frontMatter.title post.frontMatter.date
            post = post
        }
        postsDatabase.Add(post.frontMatter.slug, post')
        // printfn "****** stored %s" post.frontMatter.slug
        post'

    (*
        Todas las particularidades que fui metiendo y/o usando en las entradas de blogger
    *)
    let rec private toMarkdown (context: string) (htmlNode: HtmlAgilityPack.HtmlNode) =

        let concatChildNodes (context: string) (htmlNode: HtmlAgilityPack.HtmlNode) =
            htmlNode.ChildNodes
            |> Seq.map (toMarkdown context)
            |> String.concat ""

        let processElementType (context: string) (htmlNode: HtmlAgilityPack.HtmlNode) =
            match htmlNode.Name with
            | "blockquote" ->
                (concatChildNodes htmlNode.Name htmlNode).Split("\n")
                |> Seq.map (sprintf "> %s")
                |> String.concat "\n"
                |> sprintf "%s\n"
            | "h3" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "### %s\n\n"
            | "small" -> concatChildNodes htmlNode.Name htmlNode
            | "br" -> "\n"
            | "hr" -> "\n---\n\n"
            | "em" | "i" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "_%s_"
            | "b" | "strong" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "**%s**"
            | "u" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "__%s__"
            | "code" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "`%s`"
            | "ul" | "ol" -> concatChildNodes htmlNode.Name htmlNode
            | "li" when context = "ul" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "* %s\n"
            | "li" when context = "ol" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "1. %s\n"
            | "div" | "p" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "\n%s\n"
            | "center" -> concatChildNodes htmlNode.Name htmlNode
            | "strike" | "stroke" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "~~%s~~"
            | "a" ->
                (
                    let href =
                        htmlNode.Attributes
                        |> Seq.tryFind (fun a -> a.Name = "href")
                        |> Option.map (fun a -> a.Value)
                        |> Option.defaultValue "."

                    let finalHref =
                        if href.Contains("unomascero.blogspot.com") then
                            href.Replace("blogspot.com.es", "blogspot.com")
                            |> utils.slugging
                            |> fun s ->
                                if postsDatabase.ContainsKey(s) then
                                    // printfn "====== encontrado: %s" s
                                    let stored = postsDatabase.[s]
                                    sprintf "{{< relref \"%s\">}} \"%s\"" stored.fileName stored.post.frontMatter.title
                                else
                                    printfn "!!!!!! no encontrado: %s" s
                                    href
                        else
                            href

                    sprintf "[%s](%s)" (concatChildNodes htmlNode.Name htmlNode) finalHref
                )
            | "img" ->
                (
                    let alt =
                        htmlNode.Attributes
                        |> Seq.tryFind (fun a -> a.Name = "alt")
                        |> Option.map (fun a -> a.Value)
                        |> Option.defaultValue "image without alternative text"

                    let src =
                        htmlNode.Attributes
                        |> Seq.tryFind (fun a -> a.Name = "src")
                        |> Option.map (fun a -> a.Value)
                        |> Option.defaultValue "."

                    if context = "a" then
                        sprintf "![%s](%s)" alt src
                    else
                        sprintf "![%s](%s)\n\n" alt src
                )
            | "span" ->
                (
                    let style =
                        htmlNode.Attributes
                        |> Seq.tryFind (fun a -> a.Name = "style")
                        |> Option.map (fun a -> a.Value)
                        |> Option.defaultValue ""

                    let class' =
                        htmlNode.Attributes
                        |> Seq.tryFind (fun a -> a.Name = "class")
                        |> Option.map (fun a -> a.Value)
                        |> Option.defaultValue ""

                    if style <> "" then
                        if style.Contains("font-size") then
                            concatChildNodes htmlNode.Name htmlNode
                        else if style.Contains("bold") then
                            concatChildNodes htmlNode.Name htmlNode |> sprintf "**%s**"
                        else if style.Contains("italic") then
                            concatChildNodes htmlNode.Name htmlNode |> sprintf "__%s__"
                        else if style.Contains("color:") then
                            concatChildNodes htmlNode.Name htmlNode |> sprintf "**%s**"
                        else if style.Contains("font-family") && (style.ToLower().Contains("courier new") || style.ToLower().Contains("console")) then
                            concatChildNodes htmlNode.Name htmlNode |> sprintf "`%s`"
                        else
                            printfn "No soy capaz de procesar este span !!!! %s" htmlNode.OuterHtml
                            concatChildNodes htmlNode.Name htmlNode
                    else if class'.Contains("blsp-spelling-error") then
                        concatChildNodes htmlNode.Name htmlNode |> sprintf "~~`%s`~~"
                    else if class'.Contains("blsp-spelling-corrected") then
                        concatChildNodes htmlNode.Name htmlNode |> sprintf "__%s__"
                    else
                        printfn "No soy capaz de procesar este span !!!! %s" htmlNode.OuterHtml
                        concatChildNodes htmlNode.Name htmlNode
                )
            | "font" -> concatChildNodes htmlNode.Name htmlNode
            | "sub" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "<sub>%s</sub>"
            | "sup" -> concatChildNodes htmlNode.Name htmlNode |> sprintf "<sup>%s</sup>"
            | "pre" ->
                let content =
                    htmlNode.InnerText.Split("\n")
                    |> Seq.map (fun l -> sprintf "  %s" l)
                    |> String.concat "\n"

                content |> sprintf "\n```%s```\n\n"
            | "table" | "tbody" |  "tr" | "td" -> concatChildNodes htmlNode.Name htmlNode
            | "iframe" when htmlNode.OuterHtml.Contains("www.youtube") && htmlNode.OuterHtml.Contains(".com/embed/") ->
                let mutable src =
                    htmlNode.Attributes
                    |> Seq.tryFind (fun a -> a.Name = "src")
                    |> Option.map (fun a -> a.Value)
                    |> Option.defaultValue ""

                src <- src.Replace("-nocookie", "")
                src <- src.Replace("http:", "")
                src <- src.Replace("https:", "")
                src <- src.Replace("//", "")
                src <- src.Replace("www.youtube.com/embed/", "")
                src <-
                    if src.Contains("?rel=") then
                        src.Substring(0, src.IndexOf("?rel="))
                    else
                        src

                sprintf "\n{{< youtube %s >}}\n\n" src
            | _ ->
                printfn "No he podido entender este caso !!!! %s" htmlNode.OuterHtml
                sprintf "\n{{< unsafe-raw-html >}}\n%s\n{{< / unsafe-raw-html >}}" htmlNode.OuterHtml

        match htmlNode.NodeType with
        | HtmlAgilityPack.HtmlNodeType.Comment -> ""
        | HtmlAgilityPack.HtmlNodeType.Text -> htmlNode.InnerText
        | HtmlAgilityPack.HtmlNodeType.Element -> processElementType context htmlNode
        | HtmlAgilityPack.HtmlNodeType.Document -> concatChildNodes context htmlNode
        | _ -> "ERROR"

    let fromHtml html =
        let doc = HtmlAgilityPack.HtmlDocument()
        doc.LoadHtml(html)
        toMarkdown "" doc.DocumentNode

    let private save (storablePost: storablePost) =
        let post = storablePost.post
        let fileContent =
            [
                "---"
                sprintf "id: \"%s\"" post.frontMatter.id
                sprintf "draft: %b" post.frontMatter.draft
                sprintf "date: %s" post.frontMatter.date
                sprintf "title: \"%s\"" post.frontMatter.title
                sprintf "description: \"%s\"" post.frontMatter.description
                sprintf "slug: \"%s\"" post.frontMatter.slug
                "tags:"
                post.frontMatter.tags |> List.map (sprintf "  - \"%s\"") |> String.concat "\n"
                "categories:"
                post.frontMatter.categories |> List.map (sprintf "  - \"%s\"") |> String.concat "\n"
                sprintf "externalLink: \"%s\"" post.frontMatter.externalLink
                "series:"
                post.frontMatter.series |> List.map (sprintf "  - \"%s\"") |> String.concat "\n"
                "aliases:"
                post.frontMatter.aliases |> List.map (sprintf "  - \"%s\"") |> String.concat "\n"
                "---"
                ""
                post.content
            ] |> String.concat "\n"
        System.IO.File.WriteAllText(storablePost.fileName, fileContent, System.Text.Encoding.UTF8)

    let storeAll () =
        for p in postsDatabase.Values do save p

// ----- END MODULE hugo -----

let convertToPost (banner: string) (asDraft: bool) (entry: backup.Entry) : hugo.model.post =

    let categories' =
        entry.Categories
        |> Seq.filter (fun c -> c.Scheme = "http://www.blogger.com/atom/ns#")
        |> Seq.map (fun c -> c.Term)
        |> Seq.append ["personal"; "vidas pasadas"]
        |> Seq.toList

    let slug' =
        entry.Links
        |> Seq.tryFind (fun l -> l.Rel = "alternate")
        |> Option.map (fun l -> utils.slugging l.Href)
        |> Option.defaultValue entry.Id

(*
    let alias' =
        entry.Links
        |> Seq.tryFind (fun l -> l.Rel = "alternate")
        |> Option.map (fun l -> utils.aliased l.Href)
        |> Option.map (fun l -> [l])
        |> Option.defaultValue []
*)

    let title' =
        entry.Title.Value.Replace("\"", "'")

    {
        frontMatter = {
            id = entry.Id
            draft = asDraft
            date = entry.Published
            title = title'
            description = "post importado desde unomascero.blogspot.com"
            slug = slug'
            tags = ["recuerdos"; "unomascero"]
            categories = categories'
            externalLink = "unomascero.blogspot.com"
            series = ["vidas pasadas"; "píldoras para la egolatría"]
            aliases = []
        }
        content = hugo.fromHtml entry.Content.Value |> sprintf "%s%s" banner // entry.Content.Value
    }

[<Literal>]
let FilePath = "blog-11-06-2021.xml"

(* {{< unsafe-raw-html >}} es un shortcode de Hugo para forzar la inclusión del html que queramos. *)
[<Literal>]
let BannerEachPost = "{{< unsafe-raw-html >}}<div style=\"padding: 1em; border-top: 1px dashed black; border-bottom: 1px dashed black; background-color: lightgray;\">{{< / unsafe-raw-html >}}\n\n\
                      _Esta entrada ha sido importada desde mi anterior blog: **Píldoras para la egolatría**_\n\n\
                      Es muy probable que el formato no haya quedado bien y/o que parte del contenido, como imágenes y vídeos, \
                      no sea visible. Asimismo los enlaces probablemente funcionen mal.\n\n\
                      Por último pedir diculpas por el contenido. Es de muy mala calidad y la mayoría de las entradas recuperadas \
                      no merecían serlo. Pero aquí está esta entrada como ejemplo de que no me resulta fácil deshacerme de lo \
                      que había escrito. De verdad que lo siento muchísimo si has llegado aquí de forma accidental y te has parado \
                      a leerlo. &#x1F614;\n\
                      {{< unsafe-raw-html >}}</div>{{< / unsafe-raw-html >}}\n\n"

let feed = backup.read(FilePath)

let isPost (entry: backup.Entry) =
    entry.Categories |> Array.exists (fun c -> c.Term.EndsWith("kind#post"))

let posts =
    let asDraft = convertToPost BannerEachPost true
    let noDraft = convertToPost BannerEachPost false
    feed.Entries
    |> Seq.filter isPost
    |> Seq.rev
    // |> Seq.skip 100
    // |> Seq.take 200
    // |> Seq.toList
    |> Seq.map (noDraft >> hugo.store)
    |> Seq.toList
    |> ignore
    |> hugo.storeAll
    // |> Seq.iter hugo.save
    // |> Seq.iter (fun p -> System.Console.WriteLine(p))
