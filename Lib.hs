module Main where

import Graphics.UI.GLUT

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints =
  [ (0.5, 0.5, 0.0)
  , (0.5, -0.5, 0.0)
  , (-0.5, -0.5, 0.0)
  , (-0.5, 0.5, 0.0)
  ]

colorTriangle = do
  currentColor $= Color4 1 0 0 1
  vertex $ Vertex3 (-0.5) (-0.5) (0 :: GLfloat)
  currentColor $= Color4 0 1 0 1
  vertex $ Vertex3 0.5 (-0.5) (0 :: GLfloat)
  currentColor $= Color4 0 0 1 1
  vertex $ Vertex3 (-0.5) 0.5 (0 :: GLfloat)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  windowPosition $= Position 0 0
  windowSize $= Size 800 600
  mainLoop

display :: DisplayCallback
display = do
  clearColor $= Color4 0.2 0.3 0.3 1
  clear [ColorBuffer]
  polygonMode $= (Line, Line)
  renderPrimitive Triangles $ do
    currentColor $= Color4 1 0.5 0.2 1
    mapM_
      (\(a, b, c) -> vertex $ Vertex3 a b c)
      [ myPoints !! 0
      , myPoints !! 1
      , myPoints !! 3
      , myPoints !! 1
      , myPoints !! 2
      , myPoints !! 3
      ]

  flush