type snabbDomModule;;

type vNode;;
type document;;
type window;;
type timer;;

type propObj;;

type _ vNodeContent =
  | Text : string vNodeContent
  | V: (vNode array) vNodeContent;;

type action =
 | Juttu;;

type styleProperty =
  | BackgroundColor of string
  | Color of string;;

type 'a htmlProperty =
  | Style of (styleProperty list)
  | OnClick of 'a;;

type model = { thing: string};;


external sdClass : snabbDomModule = "snabbdom/modules/class" [@@bs.module];;
external sdStyle : snabbDomModule = "snabbdom/modules/style" [@@bs.module];;
external sdEventListeners : snabbDomModule = "snabbdom/modules/eventlisteners" [@@bs.module];;

external dom : document = "document" [@@bs.val];;
external win : window = "window" [@@bs.val];;

external getElementById : document -> string -> vNode = "" [@@bs.send]
external setTimeout : window -> (unit -> unit [@bs]) -> float -> timer = "" [@@bs.send]

external init : snabbDomModule array -> (vNode -> vNode -> vNode [@bs]) = "init" [@@bs.module "snabbdom"]

external snabbh : string -> < .. > Js.t -> ('a vNodeContent [@bs.ignore]) -> 'a -> vNode = "snabbdom/h" [@@bs.module]

external makeObj : unit -> < .. > Js.t = "" [@@bs.obj]

let messages = ref [];;

let styleHandler prop obj =
  match prop with
  | BackgroundColor color -> obj##backgroundColor #= color;
                             obj
  | Color color -> obj##color #= color;
                   obj

let handleProperties handler props =
  List.fold_right handler props (makeObj ());;

let queueAction action =
  messages := action :: !messages;
  (Js.log (messages));;

let htmlHandler prop obj =
  match prop with
  | Style styles -> obj##style #= (handleProperties styleHandler styles);
                    obj
  | OnClick action -> obj##on #= [%bs.obj {click = fun _ -> queueAction action}];
                      obj

let h_ a b c = snabbh a (makeObj ()) b c;;

let h tag props type' children = snabbh tag (handleProperties htmlHandler props) type' children;;

let patch = init [| sdClass; sdStyle; sdEventListeners |];;

let initModel = { thing = "Hello World!" };;

let view model = h_ "div" V [|
    h "div" [ Style [BackgroundColor "blue"; Color "red"]
            ; OnClick Juttu ]
            Text model.thing
  |]

let update action model =
  match action with
  | Juttu -> { model with thing = "Hello again!" };;

let container = getElementById dom "container";;

let patcher model = patch container (view model) [@bs];;

let initVTree = patch container (view initModel) [@bs];;

let testi action model = patch container (view (update action model)) [@bs];;

let rec messageLoop model tree =
  let (newModel, newTree) = match !messages with
                            | [] -> Js.log "no messages"; (model, tree)
                            | action :: t -> let newM = update action model in
                                             let newT = patch tree (view newM) [@bs] in
                                               messages := t;
                                               (newM, newT)
  in
  setTimeout win (fun () -> messageLoop newModel newTree; [@bs]) 100.0; ();;

messageLoop initModel initVTree;;
