module HaskellToolbox.Toolbox
       (breakAtFirst,
        breakAtLast,
        beforeFirst,
        afterFirst,
        beforeLast,
        afterLast,
        betweenLastAndFirst,
        mapPair,
        fileBaseName,
        interactive,
       ) where

import Data.Tuple (swap)

import Control.Monad.IO.Class

-- Functions on Lists
breakAtFirst :: Eq a => [a] -> a -> ([a],[a])
breakAtFirst xs y = break (== y) xs

breakAtLast :: Eq a => [a] -> a -> ([a],[a])
breakAtLast xs y = (swap.(mapPair reverse).(`breakAtFirst` y).reverse) xs

beforeFirst :: Eq a => [a] -> a -> [a]
beforeFirst xs y = (fst.(`breakAtFirst` y)) xs

afterFirst :: Eq a => [a] -> a -> [a]
afterFirst xs y = (tail.snd.(`breakAtFirst` y)) xs

beforeLast :: Eq a => [a] -> a -> [a]
beforeLast xs y = (init.fst.(`breakAtLast` y)) xs

afterLast :: Eq a => [a] -> a -> [a]
afterLast xs y = (snd.(`breakAtLast` y)) xs

betweenLastAndFirst :: Eq a => [a] -> (a,a) -> [a]
betweenLastAndFirst xs (l,f) = ((`beforeFirst` f).(`afterLast` l)) xs 

-- Functions on Tuples
mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

-- Functions on Filenames
fileBaseName :: FilePath -> String
fileBaseName = (`betweenLastAndFirst` ('/','.'))

-- IO Functions 
interactive :: MonadIO m => IO a
                -> (String -> Bool)
                -> (String -> m b)
                -> (String -> m c)
                -> m b
interactive prompt pred term suc = do
  liftIO prompt
  xs <- liftIO getLine
  if (pred xs == True)
     then term xs
     else do suc xs
             interactive prompt pred term suc 
