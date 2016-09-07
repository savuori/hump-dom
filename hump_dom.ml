type snabbDomModule;;

type vNode;;
type document;;
type window;;
type timer;;

type propObj;;

type _ vNodeContent =
  | Text : string vNodeContent
  | V: (vNode array) vNodeContent;;

type styleProperty =
  | BackgroundColor of string
  | Color of string;;

type 'a htmlProperty =
  | Style of (styleProperty list)
  | OnClick of ('a -> unit) * 'a;;

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

let styleHandler prop obj =
  match prop with
  | BackgroundColor color -> obj##backgroundColor #= color;
                             obj
  | Color color -> obj##color #= color;
                   obj

let handleProperties handler props =
  List.fold_right handler props (makeObj ());;


let htmlHandler prop obj =
  match prop with
  | Style styles -> obj##style #= (handleProperties styleHandler styles);
                    obj
  | OnClick (func, action) -> obj##on #= [%bs.obj {click = fun _ -> func action}];
                              obj

let h_ a b c = snabbh a (makeObj ()) b c;;

let h tag props type' children = snabbh tag (handleProperties htmlHandler props) type' children;;

let patch = init [| sdClass; sdStyle; sdEventListeners |];;


let startApp containerId viewFun updateFun startModel =
  let messages = ref [] in
  let queueAction action =
     messages := action :: !messages; in
  let container = getElementById dom containerId in
  let initVTree = patch container (viewFun startModel queueAction) [@bs] in
  let rec messageLoop model tree =
    let (newModel, newTree) = match !messages with
                              | [] -> (model, tree)
                              | actionList -> let newM = List.fold_right updateFun actionList model in
                                              let newT = patch tree (viewFun newM queueAction) [@bs] in
                                                messages := [];
                                                (newM, newT)
    in
    setTimeout win (fun () -> messageLoop newModel newTree; [@bs]) 25.0; ();
  in
  messageLoop startModel initVTree;;
