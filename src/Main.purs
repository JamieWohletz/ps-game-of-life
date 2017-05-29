module Main where

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array ((..))
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Grid (Grid, blankGrid, isAlive, seed)
import Point (Point, point)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, h1, span, table, tbody, td, tr)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!), (!))
import Prelude hiding (div)

type State = {
  gridSize :: Int,
  grid :: Grid
}

data Event = SetGrid Grid | SetGridSize Int | SeedGrid

initialState :: State 
initialState = {
  gridSize: 10,
  grid: blankGrid 10 10
}

renderGrid :: Int -> Grid -> HTML Event
renderGrid size grid = table ! className "conway-grid" $ tbody $ traverse_ buildRow (0..size)
  where 
    buildRow :: Int -> HTML Event
    buildRow rowIndex = tr ! className "row" $ do 
      traverse_ (buildCell rowIndex) (0..size)
    buildCell :: Int -> Int -> HTML Event
    buildCell rowIndex cellIndex = td ! className ("cell " <> cellClass (point cellIndex rowIndex) grid) $ text ""

cellClass :: Point -> Grid -> String
cellClass p g = transform $ isAlive p g
  where 
    transform :: Either String Boolean -> String
    transform = either (const "error") (\b -> if b then "alive" else "dead")

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event (console :: CONSOLE, random :: RANDOM | fx)
foldp (SetGrid grid) st = { state: st { grid = grid }, effects: [] } 
foldp (SetGridSize size) st = { state: st { gridSize = size, grid = blankGrid size size }, effects: [] }
foldp (SeedGrid) st@{ grid } = { state: st, effects: [ liftEff $ seed grid >>= (\g -> pure $ Just $ SetGrid g)]}

-- | Return markup from the state
view :: State -> HTML Event
view { gridSize, grid } =
  div do
    h1 $ text "Conway's Game of Life"
    span $ text "Select a grid size: "
    button #! onClick (const (SetGridSize 10)) $ text "10x10"
    button #! onClick (const (SetGridSize 20)) $ text "20x20"
    button #! onClick (const (SetGridSize 30)) $ text "30x30"
    button #! onClick (const SeedGrid) $ text "Seed"
    renderGrid gridSize grid

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects (console :: CONSOLE, random :: RANDOM | fx)) Unit
main = do
  app <- start
    { initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
-- module Main where

-- import Prelude
-- import App.Events (AppEffects, Event(..), foldp)
-- import App.Routes (match)
-- import App.State (State, init)
-- import App.View.Layout (view)
-- import Control.Monad.Eff (Eff)
-- import DOM (DOM)
-- import DOM.HTML (window)
-- import DOM.HTML.Types (HISTORY)
-- import Pux (CoreEffects, App, start)
-- import Pux.DOM.Events (DOMEvent)
-- import Pux.DOM.History (sampleURL)
-- import Pux.Renderer.React (renderToDOM)
-- import Signal ((~>))

-- type WebApp = App (DOMEvent -> Event) Event State

-- type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

-- main :: String -> State -> Eff ClientEffects WebApp
-- main url state = do
--   -- | Create a signal of URL changes.
--   urlSignal <- sampleURL =<< window

--   -- | Map a signal of URL changes to PageView actions.
--   let routeSignal = urlSignal ~> \r -> PageView (match r)

--   -- | Start the app.
--   app <- start
--     { initialState: state
--     , view
--     , foldp
--     , inputs: [routeSignal] }

--   -- | Render to the DOM
--   renderToDOM "#app" app.markup app.input

--   -- | Return app to be used for hot reloading logic in support/client.entry.js
--   pure app

-- initialState :: State
-- initialState = init "/"
