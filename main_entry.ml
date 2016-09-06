type snabbDomModule;;

type vNode;;
type document;;

type propObj;;

type _ vNodeContent =
  | Text : string vNodeContent
  | V: (vNode array) vNodeContent;;

type action =
 | Juttu
 | TokaJuttu;;

type styleProperty =
  | BackgroundColor of string
  | Color of string;;

type 'a htmlProperty =
  | Style of (styleProperty list)
  | OnClick of 'a;;


external sdClass : snabbDomModule = "snabbdom/modules/class" [@@bs.module];;
external sdStyle : snabbDomModule = "snabbdom/modules/style" [@@bs.module];;
external sdEventListeners : snabbDomModule = "snabbdom/modules/eventlisteners" [@@bs.module];;

external dom : document = "document" [@@bs.val]
external getElementById : document -> string -> vNode = "" [@@bs.send]

external init : snabbDomModule array -> (vNode -> vNode -> unit [@bs]) = "init" [@@bs.module "snabbdom"]

external snabbh : string -> < .. > Js.t -> ('a vNodeContent [@bs.ignore]) -> 'a -> vNode = "snabbdom/h" [@@bs.module]

external makeObj : unit -> < .. > Js.t = "" [@@bs.obj]

let styleHandler prop obj =
  match prop with
  | BackgroundColor color -> obj##backgroundColor #= color;
                             obj
  | Color color -> obj##color #= color;
                   obj

let handleProperties handler props =
  List.fold_right handler props (makeObj ());;


let queueAction action =
  (Js.log (action));;

let htmlHandler prop obj =
  match prop with
  | Style styles -> obj##style #= (handleProperties styleHandler styles);
                    obj
  | OnClick action -> obj##on #= [%bs.obj {click = fun _ -> queueAction action}];
                      obj

let h_ a b c = snabbh a (makeObj ()) b c;;

let h tag props type' children = snabbh tag (handleProperties htmlHandler props) type' children;;

let patch = init [| sdClass; sdStyle; sdEventListeners |];;

let vnode = h_ "div" V [|
    h "div" [ Style [BackgroundColor "blue"; Color "red"]
            ; OnClick Juttu ]
            Text "Hello World!"
  |]

let container = getElementById dom "container";;

patch container vnode [@bs]
