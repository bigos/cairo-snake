module Snake (main, shrink) where


-- imports ----------------------------------------

-- import Debug.Trace
import System.Random
import Data.List
import Data.IORef ( IORef
                  , newIORef
                  , readIORef
                  , atomicModifyIORef' )
import Control.Monad.Trans.Reader (runReaderT)
import           Foreign.Ptr (castPtr)

import qualified GI.Cairo as GICairo

import qualified GI.GLib (timeoutAdd)
import qualified GI.GLib.Constants

import qualified GI.Gtk as Gtk ( DrawingArea
                               , init
                               , containerAdd
                               , drawingAreaNew
                               , main
                               , mainQuit
                               , onWidgetDestroy
                               , onWidgetDraw
                               , onWidgetKeyPressEvent
                               , widgetGetAllocatedHeight
                               , widgetGetAllocatedWidth
                               , widgetQueueDraw
                               , widgetShowAll
                               , windowNew
                               , windowSetDefaultSize)
import GI.Gtk.Enums (WindowType(..))

import qualified GI.Gdk (getEventKeyKeyval)
import GI.Gdk.Structs.EventKey

import Graphics.Rendering.Cairo ( lineTo
                                , moveTo
                                , setLineCap
                                , setLineJoin
                                , setLineWidth
                                , setSourceRGB
                                , stroke )
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types ( Cairo(Cairo)
                                      , LineCap(..)
                                      , LineJoin(..) )

-- model ----------------------------------------

type Coordinate = (Int, Int)
type FoodItems = [Coordinate]
type Snake = [Coordinate]
type LastKey = Integer
data GameField = Move | Feeding | Collision | Pause deriving (Show, Eq)
data Model = Model {
  debugData :: [String]
  , eaten :: Int
  , foodItems :: FoodItems
  , gameField :: GameField
  , growBy :: Int
  , heading :: Heading
  , height :: Int
  , lastKey :: LastKey        -- different in Elm version
  , scale :: Int
  , snake :: Snake
  , tickInterval :: Float
  , seed :: Int
  , width :: Int
  } deriving (Show)

data Heading = HeadingLeft | HeadingUp | HeadingRight | HeadingDown | None deriving (Eq, Show)

initialModel :: Model
initialModel = Model { debugData = []
                     , eaten = 0
                     , foodItems = []
                     , gameField = Move
                     , growBy = 1
                     , heading = HeadingRight
                     , Snake.height = 400
                     , lastKey = 32
                     , Snake.scale = 25
                     , snake = [(6,7),(5,7)]
                     , tickInterval = 500
                     , seed = 1
                     , Snake.width = 600 }

-- in the above example Snake.height is explained here with following text
-- https://en.wikibooks.org/wiki/Haskell/More_on_datatypes

initGlobalModel :: IO (IORef Model)
initGlobalModel = newIORef initialModel

shrink :: Int -> Int
shrink n = if (n-1) > 0 then n-1 else 0

-- foodUnderHead is wrong
foodUnderHead :: Coordinate -> Model -> Bool
foodUnderHead c model = any id $ map (\x-> c == x) (take 1 (snake model))

foodEaten :: Model -> Bool
foodEaten model = members (foodItems model) (take 2 (snake model))

member :: Eq a => a -> [a] -> Bool
member e l = any id $ map (\x -> x == e) l

members :: Eq a => [a] -> [a] -> Bool
members ee l = any id $ map (\e -> member e l) ee

headBitSnake :: Model -> Bool
headBitSnake model = any id (map (\c -> c == hsm) (drop 1 (snake model)))
  where hsm = head (snake model)

headHitWall :: Model -> Bool
headHitWall _ = False -- TODO: finish me

detectCollision :: Model -> GameField
detectCollision model =
  if headHitWall model || headBitSnake model
  then Collision
  else gameField model

-- helpers ----------------------------------------

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GICairo.Context -> Render () -> IO ()
renderWithContext ct r = GICairo.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

getWidgetSize :: Gtk.DrawingArea -> Render (Int, Int)
getWidgetSize widget = do
  width'  <- fromIntegral <$> Gtk.widgetGetAllocatedWidth widget
  height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight widget
  return (width', height')

keyToHeading :: LastKey -> Heading
keyToHeading lk
  | lk == 65361 = HeadingLeft
  | lk == 65362 = HeadingUp
  | lk == 65363 = HeadingRight
  | lk == 65364 = HeadingDown
  | otherwise = None

ifNoneThen :: Heading -> Heading -> Heading
None `ifNoneThen` v = v
h    `ifNoneThen` _ = h

-- view ----------------------------------------

drawCanvas :: Gtk.DrawingArea -> Model -> Render ()
drawCanvas canvas model = do
  size <- getWidgetSize canvas
  let mwidth  = fromIntegral (fst size) :: Int
      mheight = fromIntegral (snd size) :: Int
      s = div (min mwidth mheight) 15

  -- drawing changes color when you press 'a' on keyboard
  if (lastKey model) == 97 then setSourceRGB 0.9 0.5 0 else setSourceRGB 0.6 0.9 0
  setLineWidth $ fromIntegral (s * 10)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  -- draw snakes food
  setSourceRGB 0.4 0.6 0.1
  setLineWidth 13
  mapM_ (\c -> moveTo (xc s c) (yc s c) >> lineTo (xc s c) (yc s c)) (foodItems model)
  stroke

  -- draw snake
  case gameField model of
    Move      -> setSourceRGB 0 0.5 1
    Collision -> setSourceRGB 1 1 0.5
    _         -> setSourceRGB 0.4 0.4 0.4
  setLineWidth 5
  moveTo (xc s (head (snake model))) (yc s (head (snake model)))
  mapM_ (\c -> lineTo (xc s c) (yc s c)) (snake model)
  stroke
  where xc s c = fromIntegral $ s * (fst c)
        yc s c = fromIntegral $ s * (snd c)

-- update ----------------------------------------

data Msg = Tick | Keypress LastKey deriving (Show)

randomCoord :: (Int, Int) -> Snake -> Int -> [Coordinate]
randomCoord size snakecc seedn = take 10 (nub (filter (\x->  (not (member x snakecc))) $ take 150 zipped))
  where xrand = maxRandoms (fst size) ((fst (head snakecc)) + seedn)
        yrand = maxRandoms (snd size) seedn
        maxRandoms m seedx = randomRs (0+2, m+2) (mkStdGen seedx)
        zipped = zip xrand yrand

cook :: Model -> Model
cook model =
  if foodEaten model
  then model { gameField = detectCollision model
             , growBy = (growBy model) + 1
             , foodItems = filter (\c -> not (foodUnderHead c model)) (foodItems model)
             , debugData = [(show ("food",foodItems model,"snake", (snake model)))]
             , eaten = (eaten model) + 1 }
  else model { gameField = detectCollision model
             , growBy = shrink (growBy model)
             , debugData = (debugData model) ++["-"] }

updateGlobalModel :: Msg -> Model -> Model
updateGlobalModel (Tick) rawModel = updateTickFields model
  where model = cook rawModel
        moreFood model' =
          if ((foodItems model') == [])
          then (randomCoord (13, 13) (snake model) (seed model))
          else foodItems model'
        updateTickFields m = m { gameField = updateGamefield False model (lastKey model)
                               , foodItems = moreFood model
                               , snake = moveSnake model (heading model) }

updateGlobalModel (Keypress kv) oldModel = updateFields model
    where model = cook oldModel
          newKv      = fromIntegral kv
          newHeading = keyToHeading newKv `ifNoneThen` heading model
          updateFields m = m { seed = succ $ seed model
                             , lastKey = newKv
                             , heading = newHeading
                             , gameField = updateGamefield True model kv
                             , snake = moveSnake model (heading model) }

updateGamefield :: (Num a, Eq a) => Bool -> Model -> a -> GameField
updateGamefield keyEvent model kk =
  if keyEvent
  then case (gameField model) of
    Pause -> Move
    Move -> if kk == 32
      then Pause
      else gameField model
    _ -> gameField model
  else gameField model

snakeGrower :: Int -> Snake -> Snake
snakeGrower growth snakecc =
  case (compare growth 0) of
    GT -> snakecc
    EQ -> init snakecc
    LT -> init $ init snakecc

moveSnake :: Model -> Heading -> Snake
moveSnake model headingv =
  if ((gameField model == Pause) || (gameField model) == Collision)
  then (snake model)
  else moveSnake2 model headingv

moveSnake2 :: Model -> Heading -> Snake
moveSnake2 model headingv =
  case headingv of
    HeadingLeft ->  (fst uhs-1, snd uhs) : snakeGrower growth snake'
    HeadingUp ->    (fst uhs, snd uhs-1) : snakeGrower growth snake'
    HeadingRight -> (fst uhs+1, snd uhs) : snakeGrower growth snake'
    HeadingDown ->  (fst uhs, snd uhs+1) : snakeGrower growth snake'
    None ->         snakeGrower growth snake'
  where snake' = snake model
        growth = growBy model
        uhs = head snake'

-- main ----------------------------------------

-- timerFun g c = (atomicModifyIORef' g (\p -> (updateGlobalModel Tick p, ()))) >>
--   Gtk.widgetQueueDraw c >>
--   return True

timerFun :: IORef Model -> Gtk.DrawingArea -> IO Bool
timerFun g c = do
  atomicModifyIORef' g $ \p -> (updateGlobalModel Tick p, ())
  Gtk.widgetQueueDraw c
  return True

drawFun :: IORef Model -> Gtk.DrawingArea -> GICairo.Context -> IO Bool
drawFun globalModel canvas context = do
  model <- readIORef globalModel
  renderWithContext context (drawCanvas canvas model)
  pure True

keyPressFun :: IORef Model -> Gtk.DrawingArea -> GI.Gdk.Structs.EventKey.EventKey -> IO Bool
keyPressFun globalModel canvas rkv = do
    kv <- GI.Gdk.getEventKeyKeyval rkv
    -- update globalModel in place
    _ <- atomicModifyIORef' globalModel $ \i -> do
      ((updateGlobalModel (Keypress (fromIntegral kv)) i), ())
    -- force redrawing of canvas widget
    Gtk.widgetQueueDraw canvas
    ov <- readIORef globalModel
    putStrLn ("You have pressed key code " ++ (show kv) ++ " " ++ (show ov))
    pure True

main :: IO ()
main = do
  _ <- Gtk.init  Nothing

  globalModel <- initGlobalModel

  win <- Gtk.windowNew WindowTypeToplevel
  Gtk.windowSetDefaultSize win 300 200
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd win canvas

  _ <- GI.GLib.timeoutAdd GI.GLib.Constants.PRIORITY_DEFAULT 250 (timerFun globalModel canvas)

  _ <- Gtk.onWidgetDraw canvas $ \context -> drawFun globalModel canvas context

  _ <- Gtk.onWidgetKeyPressEvent win $ \rkv -> keyPressFun globalModel canvas rkv

  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit

  Gtk.widgetShowAll win
  Gtk.main
