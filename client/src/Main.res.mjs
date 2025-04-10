// Generated by ReScript, PLEASE EDIT WITH CARE

import * as WebGL from "./WebGL.res.mjs";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function main() {
  var canvasMay = WebGL.querySelector("canvas");
  if (canvasMay !== undefined) {
    var glMay = WebGL.getContext(Caml_option.valFromOption(canvasMay), "webgl");
    if (glMay !== undefined) {
      var gl = Caml_option.valFromOption(glMay);
      console.log(gl);
      gl.clearColor(0.0, 0.0, 0.0, 1.0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      return ;
    }
    console.log("Unable to initialize WebGL. Your browser or machine may not support it.");
    return ;
  }
  console.log("Unable to initialize the canvas.");
}

main();

export {
  main ,
}
/*  Not a pure module */
