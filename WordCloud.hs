{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# OPTIONS_GHC -Wall #-}

module WordCloud (counts, layoutWords) where

import Prelude hiding (Word)

import qualified Data.Map.Strict as M
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.List as L


data Rotation = Horiz | Vert deriving (Show)

alt :: Rotation -> Rotation
alt Horiz = Vert
alt Vert  = Horiz

----------

data Coordinates = Coordinates { x :: Int, y :: Int } deriving (Show)

(*:) = Coordinates -- operator for convenience
infixr 5 *:

instance Num Coordinates where
    c1 + c2 = (x c1 + x c2) *: (y c1 + y c2)
    c1 - c2 = (x c1 - x c2) *: (y c1 - y c2)
    -- partial instance; other operations undefined

coordMap :: (Int -> Int) -> Coordinates -> Coordinates
coordMap f Coordinates{..} = f x *: f y

data Box = Box { topLeft :: Coordinates
               , btmRight :: Coordinates
               } deriving (Show)

size :: Box -> (Int, Int)
size Box{..} = let (Coordinates x y) = btmRight - topLeft in (x, y)

center :: Box -> Coordinates
center Box{..} = topLeft + coordMap (`div` 2) (btmRight - topLeft)

----------

data Word = Word { wrd  :: Text -- the word
                 , wLen :: Int  -- length in pixels
                 , wHit :: Int  -- height in pixels
                 } deriving Show

align :: Rotation -> Word -> Word
align Horiz w            = w
align Vert  (Word t l h) = Word t h l

counts :: [Text] -> Map Text Int
counts = L.foldl' f M.empty 
    where f :: Map Text Int -> Text -> Map Text Int
          f m k = M.insertWith (+) k 1 m


layoutWords :: Map Text Int -> [(Word, Rotation, Coordinates)]
layoutWords = layoutWords' Horiz [initialBox] . toWordsDesc
    where initialBox = Box (0 *: 0) (640 *: 480) 

layoutWords' :: Rotation -> [Box] -> [Word] -> [(Word, Rotation, Coordinates)]
layoutWords' _ _ [] = []
layoutWords' _ [] _ = []
layoutWords' r (b:bs) (w:ws) =
    let w' = align r w
     in case place b w' of
          Nothing -> []
          Just (coord, newBoxes) ->
              (w', r, coord) : layoutWords' (alt r) (bs ++ newBoxes) ws

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
