combsTo _ [] = [0]
combsTo v (x:xs) = filter (==v) $ do
  a <- [0,x..v]
  b <- combsTo (v - a) xs
  return $ a + b