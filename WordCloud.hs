module WordCloud (layoutWords) where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)

newtype Rotation    = Rotation Float
newtype Coordinates = Coordinates (Int, Int)



layoutWords :: Map Text Int -> [(Text, Rotation, Coordinates)]
layoutWords = undefined
