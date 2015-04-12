module Main where

import Graphics.UI.GLUT
import Data.IORef
import Data.List ( intercalate )
import Control.Monad ( forM_ )

type Polygon = (Color3 GLfloat, [(Vector2 GLfloat, (GLfloat, GLfloat))])
type GameMap = [Polygon]

data GameDisplayMode = FirstPerson | TopView

data GameState = GameState
    { gameDisplayMode :: IORef GameDisplayMode
    , cameraPosition :: IORef (Vector3 GLdouble)
    , cameraAngle :: IORef (GLdouble)
    , cameraZoom :: IORef (GLdouble)
    , gameMap :: IORef (GameMap)
    }

defaultExtrudeLength :: GLfloat
defaultExtrudeLength = 15.0

turnSpeed :: GLdouble
turnSpeed = 0.01

niceRed = Color3 0.8 0.2 0.2
niceGreen = Color3 0.2 0.8 0.2
niceBlue = Color3 0.2 0.2 0.8
niceYellow = Color3 0.2 0.8 0.8
nicePurple = Color3 0.8 0.8 0.2
niceBrown = Color3 0.8 0.2 0.8

niceColors = [niceRed, niceGreen, niceBlue, niceYellow, nicePurple, niceBrown]

edgeColor :: Color3 GLfloat
edgeColor = Color3 0.0 0.0 0.0

moveSpeed :: GLdouble
moveSpeed = 0.2

zoomSpeed :: GLdouble
zoomSpeed = 0.01

-- | Given a polygon represented as a list of clockwise 2D points, create a
-- 3D extrusion of that shape
extrudePolygon length points = concatMap (\(p1, p2) -> [p1, p2]) $ zip bottomPolygon topPolygon
    where topPolygon = map (\(x, z) -> (x, length, z)) points
          bottomPolygon = map (\(x, z) -> (x, -length, z)) points

defaultExtrudePolygon = extrudePolygon defaultExtrudeLength

drawPolygon (c, points) = do
    forM_ [(c, QuadStrip), (edgeColor, Lines)] $ \(c, mode) -> do
        color c
        renderPrimitive mode $
            normal $ Normal3
            mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

-- | From a given center, radius, and count of vertices
makeRegularPolygon (x, y) r n =
    [(undefined, (x + r * cos t, y + r * sin t)) | t <- [0, (pi/(n / 2))..(n*pi/(n / 2))]] -- TODO figure out how to compute the normal vector for each face

setCamera :: GameState -> IO ()
setCamera s = do
    mode <- get $ gameDisplayMode s
    case mode of
        FirstPerson -> do
            matrixMode $= Projection
            loadIdentity
            frustum (-0.1) 0.1 (-0.0001) (0.0001) 0.5 100.0

            matrixMode $= Modelview 0
            loadIdentity

            Vector3 x y z <- get $ cameraPosition s
            theta <- get $ cameraAngle s
            let offsetX = cos theta
            let offsetZ = sin theta
            putStrLn $ intercalate " " $ map show [x, y, z, theta]
            lookAt (Vertex3 x y z) (Vertex3 (x + offsetX) y (z + offsetZ)) (Vector3 0 1 0)

            let cast = fromRational . toRational

            position (Light 0) $= Vertex4 (cast x) (cast y) (cast z) 1.0

        TopView -> do
            matrixMode $= Projection
            loadIdentity
            frustum (-10) 10 (-10) (10) 0.5 100.0

            matrixMode $= Modelview 0
            loadIdentity
            get (cameraZoom s) >>= \x -> scale x x x
            lookAt (Vertex3 0 50 0) (Vertex3 0 45 0) (Vector3 1 0 0)

            position (Light 0) $= (Vertex4 0 50 0 1.0 :: Vertex4 GLfloat)

            get (cameraZoom s) >>= \x -> scale x x x

advanceCamera :: GLdouble -> GameState -> IO ()
advanceCamera distance s = do
    Vector3 x y z <- get $ cameraPosition s
    theta <- get $ cameraAngle s

    let xoffset = distance * cos theta
    let zoffset = distance * sin theta
    let newpos = (Vector3 (x + xoffset) y (z + zoffset))
    print newpos
    cameraPosition s $= newpos

input :: GameState -> KeyboardMouseCallback
input s key Down _ _ = do
    postRedisplay Nothing
    case key of
        (SpecialKey KeyLeft) -> cameraAngle s $~! (subtract turnSpeed)
        (SpecialKey KeyRight) -> cameraAngle s $~! (+ turnSpeed)
        (SpecialKey KeyUp) -> advanceCamera moveSpeed s
        (SpecialKey KeyDown) -> advanceCamera (negate moveSpeed) s
        (Char ' ') ->
            gameDisplayMode s $~! \mode -> case mode of
                TopView -> FirstPerson
                FirstPerson -> TopView
        (Char 'f') -> cameraZoom s $~! (+ zoomSpeed)
        (Char 'v') -> cameraZoom s $~! (subtract zoomSpeed)
        _ -> return ()
input _ _ _ _ _ = return ()

display :: GameState -> DisplayCallback
display s = do
    clear [ColorBuffer, DepthBuffer]

    setCamera s

    gmap <- get $ gameMap s :: IO GameMap

    mapM_ (drawPolygon . fmap defaultExtrudePolygon) gmap

    swapBuffers

makeGameMap = zip niceColors $
    [ makeRegularPolygon (1, 1) 4 4
    , makeRegularPolygon (7, 7) 3 6
    , makeRegularPolygon (-3, -1) 2 7
    , makeRegularPolygon (8, -3) 4 12
    ]

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    lighting $= Enabled
    light (Light 0) $= Enabled
    ambient (Light 0) $= Color4 0 0 0 1
    diffuse (Light 0) $= Color4 1 1 1 1
    specular (Light 0) $= Color4 1 1 1 1
    colorMaterial $= Just (Front, AmbientAndDiffuse)
    materialSpecular Front $= Color4 1 1 1 1
    materialEmission Front $= Color4 0 0 0 1
    _window <- createWindow "Hello World"
    depthFunc $= Just Less
    camPos <- newIORef (Vector3 0 0 0)
    camAngle <- newIORef 0
    camZoom <- newIORef 1.0
    gameDisplayMode <- newIORef FirstPerson
    gameMap <- newIORef makeGameMap
    let gameState = GameState { gameDisplayMode = gameDisplayMode
                              , cameraPosition = camPos
                              , cameraAngle = camAngle
                              , cameraZoom = camZoom
                              , gameMap = gameMap
                              }
    displayCallback $= display gameState
    keyboardMouseCallback $= Just (input gameState)
    mainLoop

