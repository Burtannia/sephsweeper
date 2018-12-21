module Sweeper.Utils where

-- |'map' over lists of lists.
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f xs = map (map f) xs

-- |'mapM' over lists of lists.
mapM2 :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2 _ [] = return []
mapM2 f (xs:xss) = do
    ys <- mapM f xs
    yss <- mapM2 f xss
    return $ ys : yss