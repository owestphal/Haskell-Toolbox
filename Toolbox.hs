module Toolbox
       (breakAtFirst,
        breakAtLast,
        beforeFirst,
        afterFirst,
        beforeLast,
        afterLast,
        betweenLastAndFirst,
        mapPair,
        fileBaseName,
        interactive
       ) where

import Data.Tuple (swap)

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
interactive :: IO a
               -> (String -> Bool)
               -> (String -> IO b)
               -> (String -> IO c)
               -> IO b
interactive prompt pred term suc = do
  prompt
  xs <- getLine
  if (pred xs == True)
    then term xs
    else do suc xs
            interactive prompt pred term suc
