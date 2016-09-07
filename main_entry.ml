open Hump_dom

type action =
 | Juttu;;

type model = { thing: string};;

let initModel = { thing = "Hello World!" };;


let view model = h_ "div" V [|
    h "div" [ Style [BackgroundColor "blue"; Color "red"]
            ; OnClick Juttu ]
            Text model.thing
  |]

let update action model =
  match action with
  | Juttu -> { model with thing = "Hello again!" };;

startApp "container" view update initModel
