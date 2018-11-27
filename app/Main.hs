{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Data.Bits

-- adder implemented as a normal uncurried function
adder :: Bits b => (b,b,b) -> (b,b)
adder (x,y,cin) = (sumBit,carry) where
    x1 = xor x y
    sumBit = xor x1 cin
    a1 = x1.&.cin
    a2 = x.&.y
    carry = a1.|.a2

-- adder in arrow notation using proc syntax
adder' :: (Arrow a, Bits b) => a (b,b,b) (b,b)
adder' = proc (x,y,cin) -> do
    x1 <- xor' -< (x, y)
    sumBit <- xor' -< (x1, cin)
    a1 <- and' -< (x1, cin)
    a2 <- and' -< (x, y)
    carry <- or' -< (a1, a2)
    returnA -< (sumBit, carry)
    where
        arrowize = arr . uncurry
        [xor', and', or'] = arrowize <$> [xor, (.&.), (.|.)]

-- adder in arrow notation without proc syntax
-- the following diagram may help to understand how it's modeled:
--
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

adder'' :: (Arrow a, Bits b) => a (b,b,b) (b,b)
adder'' =
    arr (\(x,y,z) -> ((x,y),z)) &&& arr (\(x,y,_) -> (x,y)) -- (((x,y),cin), (x,y))
    >>> second andA                                         -- (((x,y),cin), a2)
    >>> first (first (xorA &&& andA))                       -- (((x1,a1),cin), a2)
    >>> first (arr (\((x,y),z) -> ((x,z),y)))               -- (((x1,cin),a1), a2)
    >>> first (first xorA)                                  -- ((sumBit,a1), a2)
    >>> arr (\((x,y),z) -> (x,(y,z)))                       -- (sumBit,(a1,a2))
    >>> second orA                                          -- (sumBit,carry)
    where
        arrowize = arr . uncurry
        andA = arrowize (.&.)
        orA = arrowize (.|.)
        xorA = arrowize xor

main :: IO ()
main = undefined
