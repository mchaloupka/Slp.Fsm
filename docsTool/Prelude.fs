namespace DocsTool

open Flurl

module Uri =
    open System
    let simpleCombine (slug : string) (baseUri : Uri) =
        Url.Combine(baseUri.AbsoluteUri, slug)

    let create (url : string) =
        match Uri.TryCreate(url, UriKind.Absolute) with
        | (true, v) -> v
        | _ -> failwithf "Bad url %s" url
