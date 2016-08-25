{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Life
    ( Cell(..)
    , Neighbors(..)
    , Universe(..)
    , nextCellState
    , nextUniverseState
    ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Array.IArray (IArray, Array, Ix, (!), array, assocs, bounds)
import Data.Ix (inRange)

data Cell = Populated | Empty
    deriving (Show, Eq, Generic, NFData)

-- | The neighbors of a cell. Corresponding to:
-- [a3] [b3] [c3]
-- [a2] [b2] [c2]
-- [a1] [b1] [c1]
--
-- Where b2 is the index in question
data Neighbors a = Neighbors
    { _a3 :: Maybe a
    , _b3 :: Maybe a
    , _c3 :: Maybe a
    , _a2 :: Maybe a
    , _c2 :: Maybe a
    , _a1 :: Maybe a
    , _b1 :: Maybe a
    , _c1 :: Maybe a
    } deriving (Show, Eq)

data Universe = Universe
    { _size :: Int -- square size
    , _cells :: Array (Int, Int) Cell
    } deriving (Show, Eq, Generic, NFData)

nextCellState :: Cell -> Neighbors Cell -> Cell
nextCellState Populated neighbors
    | neighborCount neighbors < 2   = Empty
    | neighborCount neighbors < 4   = Populated
    | otherwise                     = Empty
nextCellState Empty neighbors
    | neighborCount neighbors == 3  = Populated
    | otherwise                     = Empty

neighborCount :: Neighbors Cell -> Int
neighborCount Neighbors{..} =
    int _a3 + int _b3 + int _c3 + int _a2 + int _c2 + int _a1 + int _b1 + int _c1
    where int Nothing           = 0
          int (Just Empty)      = 0
          int (Just Populated)  = 1

nextUniverseState :: Universe -> Universe
nextUniverseState universe =
    let prevCells = _cells universe
        nextCells = imap f prevCells
        f ix elem = nextCellState elem (neighbors prevCells ix)
    in
    universe { _cells = nextCells }

-- Array helpers

imap :: (IArray a e, Ix i, IArray a e')
     => (i -> e -> e')
     -> a i e
     -> a i e'
imap f xs =
    let ix = bounds xs
        inputs = zip (fst <$> assocs xs) (uncurry f <$> assocs xs)
    in array ix inputs

neighbors :: (IArray a e) => a (Int, Int) e -> (Int, Int) -> Neighbors e
neighbors a (x, y) =
    Neighbors {
          _a3 = f (x+1, y-1)
        , _b3 = f (x+1, y)
        , _c3 = f (x+1, y+1)

        , _a2 = f (x, y-1)
        , _c2 = f (x, y+1)

        , _a1 = f (x-1, y-1)
        , _b1 = f (x-1, y)
        , _c1 = f (x-1, y+1)
        }
    where f i = a !? i

(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
a !? i =
    let ix = bounds a
    in if inRange ix i
    then Just (a ! i)
    else Nothing
