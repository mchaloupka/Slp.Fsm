module Nav

open System
open DocsTool
open Fable.React
open Fable.React.Props

type NameOfArticle = string
type UrlPath = string

type TopLevelNav = {
    DocsRoot : IO.DirectoryInfo
    DocsPages : IO.FileInfo list
}

type NavConfig = {
    SiteBaseUrl : Uri
    GitHubRepoUrl : Uri
    ProjectName : string
    TopLevelNav : TopLevelNav
}

let normalizeText text =
    System.Text.RegularExpressions.Regex.Replace(text, @"[^0-9a-zA-Z\.]+", " ")

let normalizeStr =  normalizeText >> str

let navItem link inner  =
    li [
        Class "nav-item"
    ] [
        a [
            Class "nav-link"
            Href link
        ] inner
    ]

let navItemText text link =
        navItem link [ normalizeStr text ]

let navItemIconOnly link ariaLabel inner =
    li [Class "nav-item"] [
        a [
            Class "nav-link"
            HTMLAttr.Custom("aria-label", ariaLabel)
            Href link
        ] inner
    ]

let dropDownNavMenu text items =
            li [ Class "nav-item dropdown" ][
                a [
                    Id (sprintf "navbarDropdown-%s"  text)
                    Href "#"
                    DataToggle "dropdown"
                    AriaHasPopup true
                    AriaExpanded false
                    Class "nav-link dropdown-toggle" ]
                    [ normalizeStr text ]
                ul [    HTMLAttr.Custom ("aria-labelledby", "dropdownMenu1")
                        Class "dropdown-menu border-0 shadow" ] items ]

let dropDownNavItem text link =
    li [
        Class "nav-item"
    ] [
        a [
            Class "dropdown-item"
            Href link
        ] [
            normalizeStr text
        ]
    ]
let dropdownSubMenu text items =
    li [ Class "dropdown-submenu" ] [
        a [ Id (sprintf "navbarDropdown-%s"  text)
            Href "#"
            Role "button"
            DataToggle "dropdown"
            AriaHasPopup true
            AriaExpanded false
            Class "dropdown-item dropdown-toggle" ] [
                normalizeStr text ]
        ul [
            HTMLAttr.Custom ("aria-labelledby", "dropdownMenu2")
            Class "dropdown-menu border-0 shadow" ] items
    ]

type NavTree =
| File of title:string * link:string
| Folder of title: string * link:string * NavTree list

let rec sortNavTree (navtree : NavTree list) =
    navtree
    |> List.map(fun navTree ->
        match navTree with
        | File (t,l) -> File (t,l)
        | Folder(title, link, nodes) -> Folder(title, link, sortNavTree nodes)
    )
    |> List.sortBy(fun navtree ->
        match navtree with
        | File(title,_) -> title
        | Folder(title, _, _) -> title
    )

let navTreeFromPaths (rootPath : IO.DirectoryInfo) (files : IO.FileInfo list) =
    let rec addPath filePath currentPath parts nodes =
        match parts with
        | [] -> nodes
        | hp :: tp ->
            addHeadPath filePath currentPath hp tp nodes
    and addHeadPath (filePath: string) (currentPath: string) (part : string) remainingParts (nodes : NavTree list)=
        match nodes with
        | [] ->
            if part.EndsWith("html") then
                let subFilePath = filePath.Replace(rootPath.FullName, "")
                File(IO.Path.GetFileNameWithoutExtension part, subFilePath)
            else
                let subFilePath = (IO.Path.Combine(currentPath, part, "index.html")).Replace(rootPath.FullName, "")
                Folder(part, subFilePath, addPath filePath (IO.Path.Combine(currentPath, part)) remainingParts [])
            |> List.singleton
        | Folder(title, link, subnodes) :: nodes when title = part -> Folder(title, link, addPath filePath (IO.Path.Combine(currentPath, part)) remainingParts subnodes ) :: nodes
        | hn :: tn -> hn :: addHeadPath filePath currentPath part remainingParts tn

    ([], files)
    ||> List.fold(fun state file ->
        let subFilePath = file.FullName.Replace(rootPath.FullName, "")
        let pathParts = subFilePath.Split(IO.Path.DirectorySeparatorChar, StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        addPath file.FullName rootPath.FullName pathParts state
    )



let generateNavMenus siteBaseUrl (navTree : NavTree list) =
    let innerDo (navTree : NavTree list) =
        navTree
        |> List.map(fun nav ->
            match nav with
            | File (title, link) -> navItemText title (siteBaseUrl |> Uri.simpleCombine link)
            | Folder (title, link, _) ->
                if title = "Api_Reference" then
                    let rec findNamespaces curFolder expectedName =
                        match curFolder with
                        | Folder(folderName, _, subFolders) ->
                            subFolders |> List.collect (fun x -> findNamespaces x folderName)
                        | File(title, link) when title = expectedName ->
                            (link) |> List.singleton
                        | _ -> List.empty

                    let namespaces = findNamespaces nav ""
                    if namespaces |> List.length <> 1 then
                        invalidOp "Multiple namespaces not supported" |> raise
                    else
                        navItemText title (siteBaseUrl |> Uri.simpleCombine (namespaces |> List.head))
                        
                else
                    navItemText title (siteBaseUrl |> Uri.simpleCombine link)
        )
    innerDo navTree



let generateNav (navCfg : NavConfig) =
    nav [
        Class "navbar navbar-light navbar-expand-md sticky-top bg-light"
    ] [
        a [
            Class "navbar-brand"
            Href (navCfg.SiteBaseUrl |> Uri.simpleCombine "/index.html")
        ] [
            str (navCfg.ProjectName)
        ]
        button [
            Class "navbar-toggler"
            Type "button"
            DataToggle "collapse"
            HTMLAttr.Custom("data-target","#navbarNav" )
            HTMLAttr.Custom("aria-controls","navbarNav" )
            HTMLAttr.Custom("aria-expanded","false" )
            HTMLAttr.Custom("aria-label","Toggle navigation" )
        ] [
            span [Class "navbar-toggler-icon"] []
        ]
        div [   Class "collapse navbar-collapse"
                Id "navbarNav" ] [
            ul [ Class "navbar-nav mr-auto" ] [
                yield! navTreeFromPaths navCfg.TopLevelNav.DocsRoot navCfg.TopLevelNav.DocsPages |> sortNavTree |> generateNavMenus navCfg.SiteBaseUrl
            ]
            ul [ Class "navbar-nav"] [
                navItemIconOnly (string navCfg.GitHubRepoUrl) (sprintf "%s Repository on Github" navCfg.ProjectName) [
                    i [ Class "fab fa-github fa-lg fa-fw text-light"] []
                ]
            ]
        ]
    ]



