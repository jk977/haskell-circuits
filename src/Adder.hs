{-# LANGUAGE Arrows #-}

module Adder where

import Data.Bits
import Control.Arrow

-- full adder implemented as a normal uncurried function
adder :: Bits b => (b,b,b) -> (b,b)
adder (x,y,cin) = (sumBit,carry) where
    x1 = xor x y
    sumBit = xor x1 cin
    a1 = x1.&.cin
    a2 = x.&.y
    carry = a1.|.a2

-- full adder in arrow notation using proc syntax
adder' :: (Arrow a, Bits b) => a (b,b,b) (b,b)
adder' = proc (x,y,cin) -> do
    x1 <- xorA -< (x, y)
    sumBit <- xorA -< (x1, cin)
    a1 <- andA -< (x1, cin)
    a2 <- andA -< (x, y)
    carry <- orA -< (a1, a2)
    returnA -< (sumBit, carry)
    where
        [xorA, andA, orA] = arr . uncurry <$> [xor, (.&.), (.|.)]

-- full adder in arrow notation without proc syntax
-- the following diagram may help to understand how it's modeled:
--
--   x      y        cin
--   |\     |         |   
--   | \   / \        |   
--   |  \ /   ---.    |       
--   |   \       |    |   
--   |  / -----. |    |   
--   v v       | |    |   
--   xor---.   | |    |   
--    |    v   | |    |   
--    |   and<-|-|----|   
--    |    |   v v    |   
--    v    |   and    |   
--   xor<--|----|-----|   
--    |    |    |         
--    |    |    v         
--    v    ---->or        
-- sumBit       v         
--            carry        

adder'' :: (Arrow a, Bits b) => a (b,b,b) (b,b)
adder'' =
    arr (group &&& tupleInit)
    >>> (first xorA >>> xorA &&& andA) *** andA
    >>> arr assoc
    >>> second orA
    where
        group (x,y,z) = ((x,y),z)
        tupleInit (x,y,_) = (x,y)
        assoc ((x,y),z) = (x,(y,z))
        [andA, orA, xorA] = arr . uncurry <$> [(.&.), (.|.), xor]
