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

mapWithIx :: (Int -> a -> b) -> [a] -> [b]
mapWithIx = mapWithIx' 0
    where
        mapWithIx' _ _ [] = []
        mapWithIx' n f (x:xs) = f n x : mapWithIx' (n + 1) f xs

mapMWithIx :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
mapMWithIx = mapMWithIx' 0
    where
        mapMWithIx' _ _ []     = return []
        mapMWithIx' n f (x:xs) = do
            y <- f n x
            ys <- mapMWithIx' (n+1) f xs
            return $ y : ys
