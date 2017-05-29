module Grid where
  
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Point (Point, point)

type Grid = Map Point Boolean

emptyGrid :: Grid
emptyGrid = empty

isAlive :: Point -> Grid -> Either String Boolean
isAlive p g = interpret $ lookup p g
  where 
    interpret :: Maybe Boolean -> Either String Boolean
    interpret Nothing = Left $ "Error! Invalid grid point " <> show p
    interpret (Just alive) = Right alive

blankGrid :: Int -> Int -> Grid
blankGrid width height = foldl update emptyGrid points
  where 
    points :: Array Point
    points = (0..height) >>= (\y -> (0..width) >>= (\x -> pure $ point x y))
    update :: Grid -> Point -> Grid
    update g p = insert p false g

seed :: forall eff. Grid -> Eff (random :: RANDOM | eff) Grid
seed = traverse randomPixel 
  where 
    randomPixel :: forall effects. Boolean -> Eff (random :: RANDOM | effects) Boolean
    randomPixel _ = do 
      num <- random
      pure (num > 0.5)


