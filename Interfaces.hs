-------------------------------------------------------------------------------
-- |
-- Module      :  MoC Interfaces for SY and SDF
-- Copyright   :  (c) Gabriel Castro
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------
module Interfaces where
import ForSyDe.Shallow

--------------------------
-- SY/SDF Interfaces
---------------------------
syToSDF :: Signal (AbstExt a) -> Signal a
syToSDF = removePrst . removeBottom

sdfToSY :: Signal a -> Signal (AbstExt a)
sdfToSY = mapSY toPrst

-- Function definition for SDF actor
-- y = y[k-1] + T/2(x[k] + x[k-1])
tustinIntegrator [x] [y1] [x1] = [y1 + 10 * (x + x1)]

---------------------------
-- System definition
---------------------------
system :: Num a => Signal (AbstExt a) -> Signal (AbstExt a)
system sIn = sOut
  where
    x = syToSDF sIn
    sOut = sdfToSY y
    y = actor31SDF (1,1,1) 1 tustinIntegrator x y_0 x_0
    x_0 = delaySDF [0] x
    y_0 = delaySDF [0] y

-- test signals definition
sTest = signal [Prst 1, Prst 2, Abst, Prst 10, Prst 3, Abst]

{- to test the system, run the following in ghci
ghci> sTest
{1,2,_,10,3,_}
ghci> system sTest
{10,40,160,290}
-}

--------------------------
-- Helper functions
---------------------------
-- function that receives a list of SY data and removes the bottom values
removeBottom :: Signal (AbstExt a) -> Signal (AbstExt a)
removeBottom NullS        = NullS
removeBottom (Abst :- ys) = removeBottom ys
removeBottom (y :- ys)    = y :- removeBottom ys

-- remove the Prst from a single value
fromPrst :: AbstExt a -> a
fromPrst (Prst y) = y

-- add the Prst for a single value
toPrst :: a -> AbstExt a
toPrst = Prst

-- function that receives a list of SY data and removes the Prst values
removePrst :: Signal (AbstExt a) -> Signal a
removePrst = mapSY fromPrst
