type snabbDomModule;;

type vNode;;
type document;;

type propObj;;

type _ vNodeContent =
  | Text : string vNodeContent
  | V: (vNode array) vNodeContent;;


type styleProperty =
  | BackgroundColor of string
  | Color of string;;

type htmlProperty =
  | Style of (styleProperty list);;

external sdClass : snabbDomModule = "snabbdom/modules/class" [@@bs.module];;
external sdStyle : snabbDomModule = "snabbdom/modules/style" [@@bs.module];;

external dom : document = "document" [@@bs.val]
external getElementById : document -> string -> vNode = "" [@@bs.send]

external init : snabbDomModule array -> (vNode -> vNode -> unit [@bs]) = "init" [@@bs.module "snabbdom"]

external h : string -> < .. > Js.t -> 'a -> ('a vNodeContent [@bs.ignore]) -> vNode = "snabbdom/h" [@@bs.module]

external makeHtmlProperties : ?style: < .. > Js.t -> ?on: < .. > Js.t -> unit -> < .. > Js.t = "" [@@bs.obj]
external makeObj : unit -> < .. > Js.t = "" [@@bs.obj]

external makeStyleProperties : ?backgroundColor: string ->
                               ?color: string ->
                               unit ->
                               < .. > Js.t = "" [@@bs.obj]

let styleHandler prop obj =
  match prop with
  | BackgroundColor color -> obj##backgroundColor #= color;
                             obj
  | Color color -> obj##color #= color;
                   obj

let handleProperties handler props =
  List.fold_right handler props (makeObj ())

let htmlHandler prop obj =
  match prop with
  | Style styles -> obj##style #= (handleProperties styleHandler styles);
                   obj

let h_ a b c = h a (makeObj ()) b c;;

let html tag props type' children = h tag (handleProperties htmlHandler props) children type';;

let patch = init [| sdClass; sdStyle |];;

let vnode = h_ "div" [|
    html "div" [Style [BackgroundColor "blue"; Color "red"]] Text "Hello World!"
  |] V

let container = getElementById dom "container";;

patch container vnode [@bs]
