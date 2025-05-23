open WebGL

let setPositionAttribute = (gl: GL.s, buffers: GL.buffer, programInfo: GL.ProgramInfo.t) => {
  let numComponents = 2; // pull out 2 values per iteration
  let glType = GL.float(gl); // the data in the buffer is 32bit floats
  let normalize = false; // don't normalize
  let stride = 0; // how many bytes to get from one set of values to the next
  // 0 = use type and numComponents above
  let offset = 0; // how many bytes inside the buffer to start from
  GL.bindBuffer(gl, GL.Buffers.arrayBuffer(gl), buffers.position);
  GL.vertexAttribPointer(
    gl,
    programInfo.attribLocations.vertexPosition,
    numComponents,
    glType,
    normalize,
    stride,
    offset,
  );
  GL.enableVertexAttribArray(gl, programInfo.attribLocations.vertexPosition);
}

let drawScene = (gl: GL.s, programInfo: GL.ProgramInfo.t, buffers: GL.buffer) => {
    GL.clearColor(gl, 0.0, 0.0, 0.0, 1.0);

    GL.clearDepth(gl, 1.0); // Clear everything
    GL.enable(gl, GL.depthTest); // Enable depth testing
    GL.depthFunc(gl, GL.lequal); // Near things obscure far things

    // Clear the canvas before we start drawing on it.

    GL.clear(gl, BigInt.lor(GL.colorBufferBit(gl), GL.depthBufferBit(gl)));

    // Create a perspective matrix, a special matrix that is
    // used to simulate the distortion of perspective in a camera.
    // Our field of view is 45 degrees, with a width/height
    // ratio that matches the display size of the canvas
    // and we only want to see objects between 0.1 units
    // and 100 units away from the camera.

    let fieldOfView = (45 * Math.PI) / 180; // in radians
    let aspect = GL.Canvas.clientWidth(GL.Canvas.get(gl)) / GL.Canvas.clientHeight(GL.Canvas.get(gl));
    let zNear = 0.1;
    let zFar = 100.0;
    let projectionMatrix = Mat4.create();

    // note: glMatrix always has the first argument
    // as the destination to receive the result.
    Mat4.perspective(projectionMatrix, fieldOfView, aspect, zNear, zFar);

    // Set the drawing position to the "identity" point, which is
    // the center of the scene.
    let modelViewMatrix = Mat4.create();

    // Now move the drawing position a bit to where we want to
    // start drawing the square.
    Mat4.translate(
        modelViewMatrix, // destination matrix
        modelViewMatrix, // matrix to translate
        [-0.0, 0.0, -6.0],
    ); // amount to translate

  // Tell WebGL how to pull out the positions from the position
  setPositionAttribute(gl, buffers, programInfo);

  // Tell WebGL to use our program when drawing
  GL.ProgramInfo.useProgram(gl, GL.ProgramInfo.program(programInfo));

  // Set the shader uniforms
  gl.uniformMatrix4fv(
    programInfo.uniformLocations.projectionMatrix,
    false,
    projectionMatrix,
  );
  gl.uniformMatrix4fv(
    programInfo.uniformLocations.modelViewMatrix,
    false,
    modelViewMatrix,
  );

  {
    let offset = 0;
    let vertexCount = 4;
    GL.drawArrays(GL.triangleStrip(gl), offset, vertexCount);
  }
}

// Tell WebGL how to pull out the positions from the position
// buffer into the vertexPosition attribute.
