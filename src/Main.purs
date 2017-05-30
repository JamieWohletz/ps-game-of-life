module Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import ConwayGrid (ConwayGrid, blankGrid, isAlive, seed, aliveNeighborCount)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)
import DOM.Node.Element (clientWidth, clientHeight)
import Data.Array ((..))
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Int (floor)
import Data.Map (mapWithKey)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Point (Point, point)
import Pux (CoreEffects, App, EffModel, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))
import Signal.Time (every)
import Text.Smolder.HTML (table, tbody, td, tr)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))
import Prelude hiding (div)

type WebApp = App (DOMEvent -> Event) Event State

type State = {
  seeded :: Boolean,
  grid :: ConwayGrid,
  width :: Int,
  height :: Int,
  step :: Int
}

data Event = SetGrid ConwayGrid | SeedGrid | Simulate | Step Int ConwayGrid

initialState :: Int -> Int -> State 
initialState w h = {
  seeded: false,
  width: w,
  height: h,
  grid: blankGrid w h,
  step: 0
}

renderGrid :: Int -> Int -> ConwayGrid -> HTML Event
renderGrid w h grid = table ! className "conway-grid" $ tbody $ traverse_ buildRow (0..h)
  where 
    buildRow :: Int -> HTML Event
    buildRow rowIndex = tr ! className "row" $ do 
      traverse_ (buildCell rowIndex) (0..w)
    buildCell :: Int -> Int -> HTML Event
    buildCell rowIndex cellIndex = td ! className ("cell " <> cellClass (point cellIndex rowIndex) grid) $ text ""

cellClass :: Point -> ConwayGrid -> String
cellClass p g = transform $ isAlive p g
  where 
    transform :: Either String Boolean -> String
    transform = either (const "error") (\b -> if b then "alive" else "dead")

runGame :: forall eff. State -> Aff (console :: CONSOLE | eff) (Maybe Event)
runGame { seeded, grid, step }
  | seeded == false = pure $ Just SeedGrid
  | otherwise = do 
    let newGrid = simulateStep grid
    if newGrid == grid 
      then pure Nothing 
      else pure $ Just $ Step (step + 1) $ simulateStep grid

simulateStep :: ConwayGrid -> ConwayGrid
simulateStep g = mapWithKey simulate g
  where 
    simulate :: Point -> Boolean -> Boolean
    simulate pt alive =
      let alives = aliveNeighborCount pt g in
      (alive && (alives == 2 || alives == 3)) || 
      (not alive && (alives == 3))

-- | Return a new state (and effects) from each event
foldp :: forall fx. Event -> State -> EffModel State Event (console :: CONSOLE, random :: RANDOM | fx)
foldp (SetGrid grid) st = { state: st { grid = grid }, effects: [] } 
foldp (Step step grid) st = { state: st { grid = grid, step = step }, effects: []}
foldp (SeedGrid) st@{ grid } = { state: st { seeded = true }, effects: [ liftEff $ seed grid >>= (\g -> pure $ Just $ SetGrid g)]}
foldp (Simulate) st@{ step } = { state: st, effects: [ runGame st ] }

-- | Return markup from the state
view :: State -> HTML Event
view { width, height, grid, step } =
  renderGrid width height grid

-- | Start and render the app
main :: forall fx. Eff (CoreEffects (dom :: DOM, console :: CONSOLE, timer :: TIMER, random :: RANDOM | fx)) WebApp
main = do
  let animationSignal = every 1000.00
  let animation = animationSignal ~> \_ -> Simulate
  win <- window
  doc <- document win
  maybeBod <- body doc
  let bod = unsafePartial $ fromJust maybeBod
  let bodyElement = htmlElementToElement bod
  w <- clientWidth bodyElement
  h <- clientHeight bodyElement
  let gridWidth = (floor w) / 50
  let gridHeight = (floor h) / 50
  log $ show w <> " " <> show h
  app <- start
    { initialState: initialState gridWidth gridHeight
    , view
    , foldp
    , inputs: [animation]
    }

  renderToDOM "#app" app.markup app.input

  pure app