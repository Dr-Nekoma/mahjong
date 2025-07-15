open Browser

let canvasWidth = 600
let canvasHeight = 300

let pieceWidth = 60
let pieceHeight = 80

type position = {
  x: float,
  y: float,
}

type isDragging = No | Yes({draggingOffset: position})

type pieceState = {
  position: position,
  isDragging: isDragging,
}

type gameState = {piece: pieceState}

let initialGameState: gameState = {
  piece: {
    position: {
      x: 0.0,
      y: 0.0,
    },
    isDragging: No,
  },
}

type pointerState = {
  isDown: bool,
  position: position,
}

let pointerState = ref({
  isDown: false,
  position: {
    x: 0.0,
    y: 0.0,
  },
})

let nextGameState = (gameState: gameState, pointerState: pointerState) => {
  switch (gameState, pointerState) {
  | (_, {isDown: false}) => {
      piece: {
        ...gameState.piece,
        isDragging: No,
      },
    }
  | ({piece: {isDragging: Yes({draggingOffset})}}, {isDown: true}) => {
      piece: {
        ...gameState.piece,
        position: {
          x: pointerState.position.x -. draggingOffset.x,
          y: pointerState.position.y -. draggingOffset.y,
        },
      },
    }
  | ({piece: {isDragging: No}}, {isDown: true}) => {
      let isPointerOnPiece =
        pointerState.position.x >= gameState.piece.position.x &&
        pointerState.position.x <= gameState.piece.position.x +. pieceWidth->Int.toFloat &&
        pointerState.position.y >= gameState.piece.position.y &&
        pointerState.position.y <= gameState.piece.position.y +. pieceHeight->Int.toFloat

      let isDragging = if isPointerOnPiece {
        Yes({
          draggingOffset: {
            x: pointerState.position.x -. gameState.piece.position.x,
            y: pointerState.position.y -. gameState.piece.position.y,
          },
        })
      } else {
        No
      }

      {
        piece: {
          ...gameState.piece,
          isDragging,
        },
      }
    }
  }
}

let draw = (context: Canvas.context2d, image: Dom.imageElement, gameState: gameState) => {
  context->Canvas.fillStyle("white")
  context->Canvas.fillRect(
    ~x=0.0,
    ~y=0.0,
    ~width=canvasWidth->Int.toFloat,
    ~height=canvasHeight->Int.toFloat,
  )

  context->Canvas.drawImage(
    image,
    ~dx=gameState.piece.position.x,
    ~dy=gameState.piece.position.y,
    ~dWidth=pieceWidth->Int.toFloat,
    ~dHeight=pieceHeight->Int.toFloat,
  )
}

let lastFrameStartTime: ref<option<float>> = ref(None)

let rec gameLoop = (context: Canvas.context2d, image: Dom.imageElement, gameState: gameState) => {
  requestAnimationFrame(time => {
    switch lastFrameStartTime.contents {
    | None => ()
    | Some(lastTime) => Console.log2("frame time:", time -. lastTime)
    }

    lastFrameStartTime.contents = Some(time)

    let newGameState = nextGameState(gameState, pointerState.contents)
    draw(context, image, newGameState)
    gameLoop(context, image, newGameState)
  })
}

let canvasOpt =
  Dom.document
  ->Dom.querySelector("canvas")
  ->Option.flatMap(Dom.toCanvas)

canvasOpt->Option.forEach(canvas => {
  canvas->Canvas.setWidth(canvasWidth)
  canvas->Canvas.setHeight(canvasHeight)

  canvas
  ->Canvas.toElement
  ->Dom.addEventListener(
    #pointerdown(
      _ => {
        pointerState.contents = {
          ...pointerState.contents,
          isDown: true,
        }
      },
    ),
  )

  canvas
  ->Canvas.toElement
  ->Dom.addEventListener(
    #pointerup(
      _ => {
        pointerState.contents = {
          ...pointerState.contents,
          isDown: false,
        }
      },
    ),
  )

  canvas
  ->Canvas.toElement
  ->Dom.addEventListener(
    #pointermove(
      event => {
        pointerState.contents = {
          ...pointerState.contents,
          position: {
            x: event->Dom.offsetX,
            y: event->Dom.offsetY,
          },
        }
      },
    ),
  )

  canvas
  ->Canvas.getContext2d
  ->Option.forEach(context => {
    let image = Image.new()

    image
    ->Image.toElement
    ->Dom.addEventListener(
      #load(
        () => {
          gameLoop(context, image, initialGameState)
        },
      ),
    )

    image->Image.setSrc("/assets/piece_front.png")
  })
})
