open Jquery


external js_assert : Js.boolean -> unit = "assert" [@@bs.val]


let attributes_raw () =
  let jq_container = jquery "#container" in
    jq_container |> addClass (`str "test") |> ignore ;
    jq_container |> addClass (`func (fun [@bs.this] jq i s  -> s ^ "-list")) |> ignore ;
    ()

let attributes_overloaded () =
  let jq_container = jquery "#container" in
    jq_container |> setAttr (`kv ("class", "foo")) |> ignore ;
    ()

let css_test () =
  let jq_body = jquery "body" in
  let jq_target = jquery "#target" in
  let h = jq_body |> getCssByArray [|"height"; "background"|] in
    Js.log h ;
    h##background #= "lightblue" ;
    Js.log h ;
    jq_body |> setCss (`map h) |> ignore ;
    jq_target |> setHtml (`str "overwritten.") |> ignore ;
    ()

let () =
  afterLoadingHtml (fun _ ->
    attributes_raw () ;
    attributes_overloaded () ;
    css_test () ;
    ()
  )
