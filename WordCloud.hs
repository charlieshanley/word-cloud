{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wall #-}

module WordCloud (counts, layoutWords) where

import Prelude hiding (Word)

import qualified Data.Map.Strict as M
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.List as L
import           Data.Maybe (catMaybes)


data Rotation = Horiz | Vert deriving (Show)

alt :: Rotation -> Rotation
alt Horiz = Vert
alt Vert  = Horiz

----------

data Coordinates = Coordinates { x :: Int, y :: Int } deriving (Show)

-- operator for convenience
(*:) :: Int -> Int -> Coordinates
(*:) = Coordinates 
infixr 5 *:

instance Num Coordinates where
    c1 + c2 = (x c1 + x c2) *: (y c1 + y c2)
    c1 - c2 = (x c1 - x c2) *: (y c1 - y c2)
    -- partial instance; other operations undefined

data Box = Box { topLeft  :: Coordinates
               , btmRight :: Coordinates
               } deriving (Show)

size :: Box -> (Int, Int)
size Box{..} = let (Coordinates x y) = btmRight - topLeft in (x, y)

center :: Box -> Coordinates
center Box{..} = topLeft + coordMap (`div` 2) (btmRight - topLeft)

coordMap :: (Int -> Int) -> Coordinates -> Coordinates
coordMap f Coordinates{..} = f x *: f y

----------

data Word = Word { wrd  :: Text -- the word
                 , wLen :: Int  -- length in pixels
                 , wHit :: Int  -- height in pixels
                 } deriving Show

align :: Rotation -> Word -> Word
align Horiz w@Word{..} | wLen >= wHit = w
align Vert  w@Word{..} | wHit >= wLen = w
align _     (Word t l h)              = Word t h l

----------

counts :: [Text] -> Map Text Int
counts = L.foldl' f M.empty 
    where f :: Map Text Int -> Text -> Map Text Int
          f m k = M.insertWith (+) k 1 m


layoutWords :: Map Text Int -> [(Word, Rotation, Coordinates)]
layoutWords = catMaybes . snd . L.mapAccumL placeWhereCan initAcc . toWordsDesc
    where initAcc = (Horiz, [], [Box (0 *: 0) (640 *: 480)])

-- placeWhereCan :: a -> b -> (a, c)
placeWhereCan :: ( Rotation -- initial orientation
                 , [Box]    -- boxes we tried that couldn't fit word
                 , [Box]    -- boxes we haven't tried yet
                 )
              -> Word                        -- word to place
              -> ( (Rotation, [Box], [Box])  -- new accumulator
                 , Maybe (Word, Rotation, Coordinates) -- result
                 )
placeWhereCan (r, triedBs, [])          _ = ((alt r, [], triedBs), Nothing)
placeWhereCan (r, triedBs, b:untriedBs) w =
    let w' = align r w
        r' = alt r
     in case place b w' of
          Just (coord, newBs) ->
              ((r', [], triedBs ++ untriedBs ++ newBs), Just (w', r, coord))
          Nothing -> let w'' = align r' w'
                         r'' = alt r'
                      in case place b w'' of
                           Just (coord, newBs) ->
                               ( (r'', [], triedBs ++ untriedBs ++ newBs)
                               , Just (w'', r', coord)
                               )
                           Nothing -> placeWhereCan (r, b:triedBs, untriedBs) w


place :: Box -> Word -> Maybe (Coordinates, [Box])
place b@Box{..} w@Word{..}
    | b `fits` w = Just (wdCoord, newBoxes)
    | otherwise  = Nothing
    where 
        midpt    = center b
        wdCoord  = midpt - (div wLen 2 *: div wHit 2)
        newBoxes = 
            [ Box topLeft                         (x wdCoord *: y btmRight)
            , Box (x wdCoord *: y topLeft)        (x wdCoord + wLen *: y wdCoord)
            , Box (x wdCoord + wLen *: y topLeft) btmRight
            , Box (x wdCoord *: y wdCoord + wHit) (x wdCoord + wLen *: y btmRight)
            ]

fits :: Box -> Word -> Bool
fits box Word{..} = wLen <= bLen && wHit <= bHit
    where (bLen, bHit) = size box


toWordsDesc :: Map Text Int -> [Word]
toWordsDesc = fmap toWord . L.sortOn (negate . snd) . M.toList
    where
        toWord :: (Text, Int) -> Word
        toWord (t, i) = let pixels = round $ 8 * sqrt (fromIntegral i)
                         in Word t (T.length t * pixels) pixels

----------
-- a test case

testWords :: [Text]
testWords = "apple" : "banana" : "carrot" : "durian" : r 2 "endive" ++ r 3 "fig"
         ++ r 3 "grape" ++ r 8 "horseradish"
             where r = replicate

testLayout :: [(Word, Rotation, Coordinates)]
testLayout = layoutWords . counts $ testWords
