module Misc where

import Data.List

snocView :: [a] -> Maybe ([a],a)
snocView [] = Nothing
snocView xs_a
    | (xs,x) <- go xs_a
    = Just (xs,x)
  where
    go :: [a] -> ([a],a)
    go [x] = ([],x)
    go (x:xs)
        | !(xs',x') <- go xs
        = (x:xs', x')
    go [] = error "impossible"