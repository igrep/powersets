module Lib where

import           Control.Monad (filterM)


powersetRecursive :: [a] -> [[a]]
powersetRecursive [] = [[]]
powersetRecursive (x : xs) =
  [x : p | p <- ps] ++ ps
 where
  ps = powersetRecursive xs


powersetFilterM :: [a] -> [[a]]
powersetFilterM = filterM (const [True, False])


powersetFold :: [a] -> [[a]]
powersetFold =
  foldr (\x -> concatMap (\p -> [p, x : p])) [[]]
