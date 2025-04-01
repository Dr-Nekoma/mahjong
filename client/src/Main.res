open WebGL
let main = () => {
  let canvasMay = querySelector("canvas")

  switch canvasMay {
      | None =>
        Console.log("Unable to initialize the canvas.");
      | Some(canvas) =>

      let glMay = getContext(canvas, "webgl")

      switch glMay {
        | None =>
          Console.log("Unable to initialize WebGL. Your browser or machine may not support it.");
        | Some(gl) =>
          Console.log(gl);
          clearColor(gl, 0.0, 0.0, 0.0, 1.0);
          clear(gl, getColorBufferBit(gl));
      }

  }  

}
main();
