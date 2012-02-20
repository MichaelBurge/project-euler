import Data.Maybe
import Text.ParserCombinators.Parsec


import Parsing

addRow :: Row -> Row -> Row
addRow maxes newElements =
  let ele i = fromJust $ lookup i maxes
      f 0 x | null maxes = x
      f 0 x = (+) x $ ele 0
      f i x | (i+1) == (size newElements) = (+) x $ ele (i-1)
      f i x = (+) x $ max (ele $ i-1) (ele i)
  in mapWithKey f newElements

maxes rows = elems $ foldlWithKey (\b k a -> addRow b a) empty rows