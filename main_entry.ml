open Hump_dom


type action =
 | Increment
 | Decrement;;


type model = int;;


let initModel = 0;;


let view model send =
  h_ "div" V [| h "button"
                  [ OnClick (send Decrement) ]
                  Text "Decrement"

              ; h_ "div"
                   Text (string_of_int model)

              ; h "button"
                  [ OnClick (send Increment) ]
                  Text "Increment"
  |]


let update action model =
  match action with
  | Increment -> model + 1
  | Decrement -> model - 1;;


startApp "container" view update initModel
