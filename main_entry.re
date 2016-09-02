
type snabbDomModule;

type vNode;

type document;

type propObj;

type vNodeContent _ = | Text :vNodeContent string | V :vNodeContent (array vNode);

external sdClass : snabbDomModule = "snabbdom/modules/class" [@@bs.module];

external sdStyle : snabbDomModule = "snabbdom/modules/style" [@@bs.module];

external dom : document = "document" [@@bs.val];

external getElementById : document => string => vNode = "" [@@bs.send];

external init : array snabbDomModule => (vNode => vNode => unit) [@bs] = "init" [@@bs.module "snabbdom"];

external snabbHtml : string => Js.t <..> => 'a => (vNodeContent 'a) [@bs.ignore] => vNode = "snabbdom/h" [@@bs.module];


let h_ a b c => h a [%bs.obj {a: "5"}] b c;

let patch = init [|sdClass, sdStyle|];

let vnode = h_ "div" [|h "div" [%bs.obj {style: {backgroundColor: "green"}}] "Hello World!" Text|] V;

let container = getElementById dom "container";

patch container vnode [@bs];
