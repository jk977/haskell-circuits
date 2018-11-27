{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Data.Bits

adder :: (Arrow a, Bits b) => a (b,b,b) (b,b)
adder cin = proc (x,y,cin) -> do
    x1 <- xor' -< (x, y)
    sumBit <- xor' -< (x1, cin)
    a1 <- and' -< (x1, cin)
    a2 <- and' -< (x, y)
    carry <- or' -< (a1, a2)
    returnA -< (sumBit, carry)
    where
        arrowize = arr . uncurry
        [xor', and', or'] = arrowize <$> [xor, (.&.), (.|.)]

adder' :: Bits b => (b,b,b) -> (b,b)
adder' (x,y,cin) = (sumBit,carry) where
    x1 = xor x y
    sumBit = xor x1 cin
    a1 = x1.&.cin
    a2 = x.&.y
    carry = a1.|.a2

-- cin     x      y
--  |      |\     |  
--  |      | \   / \  
--  |      |  \ /   ---.
--  |      |   \       |
--  |      |  / -----. |
--  |      v v       | |
--  |      xor---.   | |
--  |       |    v   | |
--  |-------|->(.&.) | |
--  |       v    |   v v
--  `----->xor   \  (.&.)
--          |     -.  |
--          |       v v
--          v      (.|.)
--       sumBit
adder'' =
    arr group
    >>> first (arr (uncurry xor) &&& arr (uncurry (.&.)))
    >>> first (first (arr (xor cin) &&& arr (.&.cin)))
    >>> first (first (second . arr . uncurry $ (.|.)))
    >>> first (second (arr const) *** first (second $ arr const))
    >>> arr (uncurry (.|.))
    where
        group (x,y,z) = ((x,y),z)

main :: IO ()
main = undefined
