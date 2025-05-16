open WebGL

let vsSource = `
    attribute vec4 aVertexPosition;
    uniform mat4 uModelViewMatrix;
    uniform mat4 uProjectionMatrix;
    void main() {
      gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
    }
`;

let fsSource = `
  void main() {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  }
`;

let loadShader = (gl: GL.s, typ: GL.constant, src: string): option<GL.Shader.t>  => {
  open WebGL.GL

  switch Shader.create(gl, typ) {
    | None => Console.log("Unable to create shader."); None;
    | Some(shader) =>

      Shader.source(gl, shader, src);

      Shader.compile(gl, shader);

      if (!Shader.getParameter(gl, shader, Shader.compileStatus(gl))) {
        Console.log("Failed in getParameter function call");
        Shader.delete(gl, shader);
        None;
      } else {
        Some(shader);
      }
  }
}

let initShaderProgram = (gl: GL.s, vsSource: string, fsSource: string): option<GL.ProgramInfo.t> => {
  open WebGL.GL

  let vertexShaderMay = loadShader(gl, Shader.vertex(gl), vsSource);
  let fragmentShaderMay = loadShader(gl, Shader.fragment(gl), fsSource);

  switch (vertexShaderMay, fragmentShaderMay) {
    | (Some(vertexShader), Some(fragmentShader)) =>
      let shaderProgram = ProgramInfo.create(gl);
      attachShader(gl, shaderProgram, vertexShader);
      attachShader(gl, shaderProgram, fragmentShader);
      ProgramInfo.link(gl, shaderProgram);

      if (!ProgramInfo.getParameter(gl, shaderProgram, Shader.linkStatus(gl))) {
        Console.log("Failed in getParameter function call");
        None;
      } else {
        Some(shaderProgram);
      }
    | _ =>
      Console.log("Failed creating shaders");
      None;
  }
}

let main = () => {
  let canvasMay = DOM.querySelector("canvas")

  switch canvasMay {
      | None =>
        Console.log("Unable to initialize the canvas.");
      | Some(canvas) =>

      let glMay = DOM.getContext(canvas, "webgl")

      switch glMay {
        | None =>
          Console.log("Unable to initialize WebGL. Your browser or machine may not support it.");
        | Some(gl) =>
          Console.log(gl);
          GL.clearColor(gl, 0.0, 0.0, 0.0, 1.0);
          GL.clear(gl, GL.Buffers.colorBufferBit(gl));

          switch initShaderProgram(gl, vsSource, fsSource) {
            | None =>
              Console.log("Could not init shader program");
            | Some(shaderProgram) =>
              open Option
              let programInfo = {
                "program": shaderProgram,
                "attribLocations": {
                  "vertexPosition": GL.ProgramInfo.getAttribLocation(gl, shaderProgram, "aVertexPosition"),
                },
                "uniformLocations": {
                  "projectionMatrix": getExn(GL.ProgramInfo.getUniformLocation(gl, shaderProgram, "uProjectionMatrix")),
                  "modelViewMatrix": getExn(GL.ProgramInfo.getUniformLocation(gl, shaderProgram, "uModelViewMatrix")),
                },
              };
              Console.log(programInfo);
          }
      }
  }

}
main();
