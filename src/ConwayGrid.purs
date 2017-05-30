module ConwayGrid where
  
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array ((..), snoc, length, filter)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Point (Point, point, adjacents)

type ConwayGrid = Map Point Boolean

emptyGrid :: ConwayGrid
emptyGrid = empty

at :: Point -> ConwayGrid -> Maybe Boolean
at p g = lookup p g

isAlive :: Point -> ConwayGrid -> Either String Boolean
isAlive p g = interpret $ lookup p g
  where 
    interpret :: Maybe Boolean -> Either String Boolean
    interpret Nothing = Left $ "Error! Invalid grid point " <> show p
    interpret (Just alive) = Right alive

blankGrid :: Int -> Int -> ConwayGrid
blankGrid width height = foldl update emptyGrid points
  where 
    points :: Array Point
    points = (0..height) >>= (\y -> (0..width) >>= (\x -> pure $ point x y))
    update :: ConwayGrid -> Point -> ConwayGrid
    update g p = insert p false g

seed :: forall eff. ConwayGrid -> Eff (random :: RANDOM | eff) ConwayGrid
seed = traverse randomPixel 
  where 
    randomPixel :: forall effects. Boolean -> Eff (random :: RANDOM | effects) Boolean
    randomPixel _ = do 
      num <- random
      pure (num > 0.5)

deadNeighborCount :: Point -> ConwayGrid -> Int
deadNeighborCount pt g = length $ filter (\b -> not b) (neighbors pt g)

aliveNeighborCount :: Point -> ConwayGrid -> Int
aliveNeighborCount pt g = length $ filter (\b -> b) (neighbors pt g)


neighbors :: Point -> ConwayGrid -> Array Boolean
neighbors pt g = foldl reducer [] $ adjacents pt
  where 
    reducer :: Array Boolean -> Point -> Array Boolean
    reducer bs currentPoint =
      case (at currentPoint g) of
        Nothing -> bs
        Just b -> bs `snoc` b
