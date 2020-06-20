namespace DocsTool

open Flurl

module Uri =
    open System
    let simpleCombine (slug : string) (baseUri : Uri) =
        let (slugToProcess, queryParam) = 
            if slug.Contains('?') then
                let indexOfQuestionMark = slug.IndexOf('?')
                slug.Substring(0, indexOfQuestionMark), slug.Substring(indexOfQuestionMark)
            else
                slug, ""
        
        (Url(baseUri), slugToProcess.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries))
        ||> Array.fold (
            fun cur next -> cur.AppendPathSegment(next)
        )   
        |> fun x -> x.Path
        |> fun x ->
            if queryParam |> String.IsNullOrEmpty then
                x
            else
                sprintf "%s%s" x queryParam

    let create (url : string) =
        match Uri.TryCreate(url, UriKind.Absolute) with
        | (true, v) -> v
        | _ -> failwithf "Bad url %s" url
