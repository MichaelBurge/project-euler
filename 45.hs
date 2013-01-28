import Common.Sequence
import Data.Set
buf = 100000
tris  = fromAscList $ take buf triangles
pents = fromAscList $ take buf pentagonals
hexes = fromAscList $ take buf hexagonals

vals = tris `intersection` (pents `intersection` hexes)
main = putStrLn $ show $ take 3 $ elems vals