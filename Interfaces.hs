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

---------------------------
-- SY/SDF Interfaces
---------------------------
syToSDF :: Signal (AbstExt a) -> Signal a
syToSDF = removePrst . removeBottom

sdfToSY :: Signal a -> Signal (AbstExt a)
sdfToSY = mapSY toPrst

---------------------------
-- SDF within SY Domain 
---------------------------
sdfInsideSYIn :: Integral a => Signal (AbstExt a) -> (Signal a, [Int])
sdfInsideSYIn x = (syToSDF x, findBottom x)

sdfInsideSYOut :: Signal a -> [Int] -> Signal (AbstExt a)
sdfInsideSYOut x y = insertBottoms (sdfToSY x) y

-- Function definition for SDF actor
-- y = y[k-1] + T/2(x[k] + x[k-1])
tustinIntegrator [x] [y1] [x1] = [y1 + 10 * (x + x1)]

---------------------------
-- System definition case 1
---------------------------
system1 :: Integral a => Signal (AbstExt a) -> Signal (AbstExt a)
system1 sIn = sOut
  where
    x = syToSDF sIn
    sOut = sdfToSY y
    y = actor31SDF (1,1,1) 1 tustinIntegrator x y_0 x_0
    x_0 = delaySDF [0] x
    y_0 = delaySDF [0] y
    
---------------------------
-- System definition case 2
---------------------------
system2 :: Integral a => Signal (AbstExt a) -> Signal (AbstExt a)
system2 sIn = sOut
  where
    (x,t) = sdfInsideSYIn sIn
    sOut = sdfInsideSYOut y t
    y = actor31SDF (1,1,1) 1 tustinIntegrator x y_0 x_0
    x_0 = delaySDF [0] x
    y_0 = delaySDF [0] y

-- test signals definition
sTest = signal [Prst 1, Prst 2, Abst, Prst 10, Prst 3, Abst]
sTest1 = signal [Prst 1, Prst 0, Abst, Prst 0, Prst 1, Abst]

{- to test the system, run the following in ghci
ghci> sTest
{1,2,_,10,3,_}
ghci> system1 sTest
{10,40,160,290}
ghci> system2 sTest
{10,40,_,160,290,_}
-}

---------------------------
-- System definition case 3
---------------------------

-- Detector 
nextState _ [x] = x

times2 [x] = [x * 2]
times3 [x] = [x * 3]

scenario0 = (1,1, times2)  
scenario1 = (1,1, times3) 

selectScenario 0 = (1, [scenario0])
selectScenario 1 = (1, [scenario1])

d_1 = detector11SADF consume_rate nextState selectScenario initial_state 
  where
    consume_rate = 1 
    initial_state = 0

system3 :: Signal (AbstExt Integer) -> Signal (AbstExt Integer) -> Signal (AbstExt Integer)
system3 sIn sControl = sOut
  where
    (x,t) = sdfInsideSYIn sIn
    sOut = sdfInsideSYOut y t
    sdfControl = syToSDF sControl
    s_k = d_1 sdfControl
    y = kernel11SADF s_k x

{- to test the system, run the following in ghci
ghci> sTest
{1,2,_,10,3,_}
ghci> sTest1
{1,0,_,0,1,_}
ghci> system3 sTest sTest1
{3,4,_,20,9,_}
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

-- TODO: ForSyDe already has this funtion (abstExt)
-- add the Prst for a single value
toPrst :: a -> AbstExt a
toPrst = Prst

-- function that receives a list of SY data and removes the Prst values
removePrst :: Signal (AbstExt a) -> Signal a
removePrst = mapSY fromPrst

-- infinite signal
infSig = infiniteS (+1) 0
-- bottom signal
bottomSig = signal [Abst]

-- function that find the tags of bottom values from a signal 
-- and returns a list with the indexes
findBottom :: Integral a => Signal (AbstExt a) -> [Int]
findBottom xs = [i | (x,i) <- fromSignal $ zipSYa xs infSig, x == Abst]

-- ZipSY with an input signal of the type AbstExt
zipSYa :: Signal (AbstExt a) -> Signal b -> Signal (AbstExt a, b)
zipSYa (x:-xs) (y:-ys) = (x, y) :- zipSYa xs ys
zipSYa _       _       = NullS

-- function that insert bottom values based on a list of indexes
insertBottoms :: Signal (AbstExt a) -> [Int] -> Signal (AbstExt a)
insertBottoms x [] = x
insertBottoms x (y:ys) = insertBottoms (addBottom $ splitAtSY y x) ys

-- add a single bottom value between two signals
addBottom :: (Signal (AbstExt a), Signal (AbstExt a)) -> Signal (AbstExt a)
addBottom (x,y) = x +-+ bottomSig +-+ y

-- function that separates a signal into two based on an index
splitAtSY :: Int -> Signal (AbstExt a) -> (Signal (AbstExt a), Signal (AbstExt a))
splitAtSY n xs = (takeS n xs, dropS n xs)
