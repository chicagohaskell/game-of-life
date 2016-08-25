module Main where

import Life
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Array.IArray (listArray)

iter :: (a -> a) -> a -> Int -> a
iter f x 0 = x
iter f x n = iter f (f x) (n-1)

blinker :: Universe
blinker =
    Universe
    5
    (listArray ((0, 0), (4, 4))
        [ Empty, Empty, Empty, Empty, Empty
        , Empty, Empty, Empty, Empty, Empty
        , Empty, Populated,  Populated,  Populated,  Empty
        , Empty, Empty, Empty, Empty, Empty
        , Empty, Empty, Empty, Empty, Empty
        ])

oneHundred :: Universe
oneHundred =
    Universe
    100
    (listArray ((0, 0), (99, 99))
        (replicate (100 * 100) Empty))

forty96 :: Universe
forty96 =
    Universe
    4096
    (listArray ((0, 0), (4095, 4095))
        (replicate (4096 * 4096) Empty))

main :: IO ()
main = do
    _ <- evaluate (force (iter nextUniverseState forty96 2))
    return ()
