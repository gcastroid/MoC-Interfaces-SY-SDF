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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interfaces where
import ForSyDe.Shallow
import Data.Bits

---------------------------
-- SY/SDF Interfaces
---------------------------
syToSDF :: Signal (AbstExt a) -> Signal a
syToSDF = removePrst . removeAbst

sdfToSY :: Signal a -> Signal (AbstExt a)
sdfToSY = mapSY abstExt

---------------------------
-- SDF within SY Domain 
---------------------------
sdfInsideSYIn :: Signal (AbstExt a) -> (Signal a, Signal Bool)
sdfInsideSYIn x = (syToSDF x, removePrst $ findAbst x)

sdfInsideSYOut :: Signal a -> Signal Bool -> Signal (AbstExt a)
sdfInsideSYOut x = insertAbst (sdfToSY x)

-- Function definition for SDF actor
-- y = y[k-1] + T/2(x[k] + x[k-1])
tustinIntegrator [x] [y1] [x1] = [y1 + 10 * (x + x1)]

---------------------------
-- System definition case 1
---------------------------
system1 :: Num a => Signal (AbstExt a) -> Signal (AbstExt a)
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
system2 :: Num a => Signal (AbstExt a) -> Signal (AbstExt a)
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
removeAbst :: Signal (AbstExt a) -> Signal (AbstExt a)
removeAbst NullS        = NullS
removeAbst (Abst :- ys) = removeAbst ys
removeAbst (y :- ys)    = y :- removeAbst ys

-- remove the Prst from a single value
fromPrst :: AbstExt a -> a
fromPrst (Prst y) = y

-- TODO: ForSyDe already has this funtion (abstExt)
-- add the Prst for a single value
-- toPrst :: a -> AbstExt a
-- toPrst = Prst

-- function that receives a list of SY data and removes the Prst values
removePrst :: Signal (AbstExt a) -> Signal a
removePrst = mapSY fromPrst

-- function that outputs a signal with the position of the bottom values from an input signal
findAbst :: Signal (AbstExt a) -> Signal (AbstExt Bool)
findAbst NullS        = NullS
findAbst (Abst :- ys) = Prst False :- findAbst ys
findAbst (y :- ys)    = Prst True :- findAbst ys

-- function that insert bottom values based on a signal with the position of the bottom values
insertAbst :: Signal (AbstExt a) -> Signal Bool -> Signal (AbstExt a)
insertAbst _         NullS     = NullS
insertAbst NullS     (y :- ys) = if y then NullS else Abst :- NullS
insertAbst (x :- xs) (y :- ys) = if y 
                                   then x :- insertAbst xs ys 
                                   else Abst :- insertAbst (unitS x +-+ xs) ys

--------------------------
-- Reconfigurable Risc-V
--------------------------

data Dir = ADD4 | TARGET deriving (Show,Eq)
data AluOP = ADD | SUB | AND | OR | SLT deriving (Show,Eq)
data AluSRC = REG | IMM deriving (Show,Eq)
data RegSRC = ALU | PC | MEM deriving (Show,Eq)
data Instruction
  -- R-type
  = Add   Register Register Register
  | Sub   Register Register Register
  | AndBW Register Register Register
  | OrBW  Register Register Register
  | Slt   Register Register Register
  -- I-type
  | Addi Register Register Int
  | Lw   Register Register Int
  -- B/S-type
  | Sw   Register Register Int
  | Beq  Register Register Int
  -- J-type
  | Jal  Register Int deriving (Show, Eq)

type RegWR = Bool
type MemWR = Bool
type Register = Int

--------------------------
-- Program Counter
--------------------------
pcAdd4 :: AbstExt Int -> AbstExt Int
pcAdd4 (Prst pc) = abstExt $ pc + 4
pcAdd4 Abst      = Abst

pcAddi :: AbstExt Int -> AbstExt Int -> AbstExt Int
pcAddi (Prst pc) (Prst imm) = abstExt $ pc + imm
pcAddi (Prst pc) Abst       = abstExt pc
pcAddi Abst      _          = Abst

pcMux :: AbstExt Int -> AbstExt Int -> AbstExt Dir -> AbstExt Int
pcMux (Prst data1) _            (Prst ADD4) = abstExt data1
pcMux Abst         _            (Prst ADD4) = Abst
pcMux _            (Prst data2) (Prst TARGET) = abstExt data2
pcMux _            Abst         (Prst TARGET) = Abst
pcMux _            _            Abst       = Abst

p_pcInit = delaySY $ abstExt 0
p_pcMux = comb3SY pcMux
p_pcAdd4 = combSY pcAdd4
p_pcAddi = comb2SY pcAddi

--------------------------
-- Instruction Decoder
--------------------------
decode1 :: AbstExt Instruction -> (AbstExt Register, AbstExt Register, AbstExt Register, AbstExt Int)
decode1 (Prst (Add   rd rs1 rs2))  = (Prst rd, Prst rs1, Prst rs2,   Prst 0)
decode1 (Prst (Sub   rd rs1 rs2))  = (Prst rd, Prst rs1, Prst rs2,   Prst 0)
decode1 (Prst (AndBW rd rs1 rs2))  = (Prst rd, Prst rs1, Prst rs2,   Prst 0)
decode1 (Prst (OrBW  rd rs1 rs2))  = (Prst rd, Prst rs1, Prst rs2,   Prst 0)
decode1 (Prst (Slt   rd rs1 rs2))  = (Prst rd, Prst rs1, Prst rs2,   Prst 0)
decode1 (Prst (Addi  rd rs1 imm))  = (Prst rd, Prst rs1,   Prst 0, Prst imm)
decode1 (Prst (Lw    rd rs1 imm))  = (Prst rd, Prst rs1,   Prst 0, Prst imm)
decode1 (Prst (Sw    rs1 rs2 imm)) = ( Prst 0, Prst rs1, Prst rs2, Prst imm)
decode1 (Prst (Beq   rs1 rs2 imm)) = ( Prst 0, Prst rs1, Prst rs2, Prst imm)
decode1 (Prst (Jal   rd imm))      = (Prst rd,   Prst 0,   Prst 0, Prst imm)
decode1 Abst                       = (   Abst,     Abst,     Abst,     Abst)

decode2 :: AbstExt Instruction -> (AbstExt RegWR, AbstExt AluOP, AbstExt AluSRC, AbstExt RegSRC, AbstExt MemWR)
decode2 (Prst (Add   rd rs1 rs2))  = ( Prst True, Prst ADD, Prst REG, Prst ALU, Prst False)
decode2 (Prst (Sub   rd rs1 rs2))  = ( Prst True, Prst SUB, Prst REG, Prst ALU, Prst False)
decode2 (Prst (AndBW rd rs1 rs2))  = ( Prst True, Prst AND, Prst REG, Prst ALU, Prst False)
decode2 (Prst (OrBW  rd rs1 rs2))  = ( Prst True, Prst  OR, Prst REG, Prst ALU, Prst False)
decode2 (Prst (Slt   rd rs1 rs2))  = ( Prst True, Prst SLT, Prst REG, Prst ALU, Prst False)
decode2 (Prst (Addi  rd rs1 imm))  = ( Prst True, Prst ADD, Prst IMM, Prst ALU, Prst False)
decode2 (Prst (Lw    rd rs1 imm))  = ( Prst True, Prst ADD, Prst IMM, Prst MEM, Prst False)
decode2 (Prst (Sw    rs1 rs2 imm)) = (Prst False, Prst ADD, Prst REG, Prst ALU,  Prst True)
decode2 (Prst (Beq   rs1 rs2 imm)) = (Prst False, Prst SUB, Prst REG, Prst ALU, Prst False)
decode2 (Prst (Jal   rd imm))      = ( Prst True, Prst ADD, Prst REG,  Prst PC, Prst False)
decode2 Abst                       = (      Abst,     Abst,     Abst,     Abst,       Abst)

decode3 :: AbstExt Instruction -> AbstExt Bool -> AbstExt Dir
decode3 (Prst (Add   {})) _        = Prst ADD4
decode3 (Prst (Sub   {})) _        = Prst ADD4
decode3 (Prst (AndBW {})) _        = Prst ADD4
decode3 (Prst (OrBW  {})) _        = Prst ADD4
decode3 (Prst (Slt   {})) _        = Prst ADD4
decode3 (Prst (Addi  {})) _        = Prst ADD4
decode3 (Prst (Lw    {})) _        = Prst ADD4
decode3 (Prst (Sw    {})) _        = Prst ADD4
decode3 (Prst (Beq   {})) (Prst z) = if z then Prst TARGET else Prst ADD4
decode3 (Prst (Beq   {})) Abst     = Prst ADD4
decode3 (Prst (Jal   {})) _        = Prst TARGET
decode3 Abst              _        = Abst

p_decoder1 = combSY decode1
p_decoder2 = combSY decode2
p_decoder3 = comb2SY decode3

--------------------------
-- Instruction Memory
--------------------------
rom :: AbstExt Int -> AbstExt Instruction
rom (Prst 0)  = Prst (Addi 5 0 1)    -- addi t5, zero, 1
rom (Prst 4)  = Prst (Sw   0 0 0)    -- sw zero, 0(zero)
rom (Prst 8)  = Prst (Sw   0 5 0)    -- sw   t5, 0(zero)
rom (Prst 12) = Prst (Add  6 0 5)    -- add  t6, zero, t5
rom (Prst 16) = Prst (Sw   0 6 0)    -- sw   t6, 0(zero)
-- L1
rom (Prst 20) = Prst (Add  7 6 5)    -- add t7, t6, t5
rom (Prst 24) = Prst (Sw   0 7 0)    -- sw  t7, 0(zero)
rom (Prst 28) = Prst (Add  5 0 6)    -- add t5, zero, t6
rom (Prst 32) = Prst (Add  6 0 7)    -- add t6, zero, t7
--rom (Prst 36) = Prst (Jal 0 (-16)) -- jal zero, L1
rom (Prst 36) = Prst (Beq 0 0 (-16)) -- beq zero, zero, L1
rom _ = Prst (Add  0 0 0)            -- nop

p_iMem = combSY rom

--------------------------
-- Data Memory
--------------------------
writeMem :: AbstExt (Vector Int) -> AbstExt Int -> AbstExt Int -> AbstExt MemWR -> AbstExt (Vector Int)
writeMem (Prst v) (Prst addr) (Prst newData) (Prst True) = if addr > 0 && addr < 256 then
                                                           abstExt $ replaceV v addr newData
                                                           else abstExt v
writeMem (Prst v) Abst _    (Prst True)  = abstExt v
writeMem (Prst v) _    Abst (Prst True)  = abstExt v
writeMem (Prst v) _    _    (Prst False) = abstExt v
writeMem (Prst v) _    _    Abst         = abstExt v
writeMem Abst     _    _    _            = Abst

readMem :: AbstExt (Vector Int) -> AbstExt Int -> AbstExt Int
readMem (Prst v) (Prst addr1) = if addr1 >= 0 && addr1 < 1024  then
                                Prst $ atV v addr1
                                else error "Data memory does not have this address"
readMem (Prst v) Abst = Abst
readMem Abst     _    = Abst

dataMem = vector $ replicate 1024 0

p_dataMem = delaySY $ abstExt dataMem
p_dataMemNew = comb4SY writeMem
p_dataMemRead = comb2SY readMem

--------------------------
-- ALU
--------------------------
alu :: AbstExt Int -> AbstExt Int -> AbstExt AluOP -> AbstExt Int
alu (Prst data1) (Prst data2) (Prst ADD) = abstExt $ data1 + data2
alu Abst         _            (Prst ADD) = Abst
alu (Prst data1) (Prst data2) (Prst SUB) = abstExt $ data1 - data2
alu Abst         _            (Prst SUB) = Abst
alu (Prst data1) (Prst data2) (Prst AND) = abstExt $ (.&.) data1 data2
alu Abst         _            (Prst AND) = Abst
alu (Prst data1) (Prst data2) (Prst OR)  = abstExt $ (.|.) data1 data2
alu Abst         _            (Prst OR)  = Abst
alu (Prst data1) (Prst data2) (Prst SLT) = if data1 <= data2 then Prst 1 else Prst 0
alu Abst         _            (Prst SLT) = Abst
alu _            _            Abst       = Abst

aluZero :: AbstExt Int -> AbstExt Bool
aluZero (Prst res) = if res == 0 then Prst True else Prst False
aluZero Abst       = Prst False

p_alu = comb3SY alu
p_aluZero = combSY aluZero

--------------------------
-- Multiplexers
--------------------------
aluMux :: AbstExt Int -> AbstExt Int -> AbstExt AluSRC -> AbstExt Int
aluMux (Prst data1) _            (Prst REG) = abstExt data1
aluMux Abst         _            (Prst REG) = Abst
aluMux _            (Prst data2) (Prst IMM) = abstExt data2
aluMux _            Abst         (Prst IMM) = Abst
aluMux _            _            Abst       = Abst

p_aluMux = comb3SY aluMux

regMux :: AbstExt Int -> AbstExt Int -> AbstExt Int -> AbstExt RegSRC -> AbstExt Int
regMux (Prst data1) _            _            (Prst ALU) = abstExt data1
regMux Abst         _            _            (Prst ALU) = Abst
regMux _            (Prst data2) _            (Prst PC)  = abstExt data2
regMux _            Abst         _            (Prst PC)  = Abst
regMux _            _            (Prst data3) (Prst MEM) = abstExt data3
regMux _            _            Abst         (Prst MEM) = Abst
regMux _            _            _            Abst       = Abst

p_regMux = comb4SY regMux

--------------------------
-- Fib Gen Testing
--------------------------
testFib :: AbstExt Int -> AbstExt MemWR -> AbstExt Int
testFib (Prst data1) (Prst True) = case data1 of
  0 -> abstExt data1
  1 -> abstExt data1
  2 -> abstExt data1
  3 -> abstExt data1
  5 -> abstExt data1
  8 -> abstExt data1
  13 -> abstExt data1
  21 -> abstExt data1
  34 -> abstExt data1
  55 -> abstExt data1
  89 -> abstExt data1
  144 -> abstExt data1
  233 -> abstExt data1
  377 -> abstExt data1
  610 -> abstExt data1
  987 -> abstExt data1
  _ -> Abst
testFib _ (Prst False) = Abst
testFib _ Abst = Abst

p_testFib = comb2SY testFib

---------------------------
-- Register file using SADF
---------------------------
writeReg32 :: [Vector Int] -> [Register] -> [Int] -> [RegWR] -> [Vector Int]
writeReg32 [regFile] [addr] [newData] [True] = [if addr > 0 && addr < 32 then
                                                replaceV regFile addr newData
                                                else regFile]
writeReg32 [regFile] _ _ [False] = [regFile]

writeReg16 :: [Vector Int] -> [Register] -> [Int] -> [RegWR] -> [Vector Int]
writeReg16 [regFile] [addr] [newData] [True] = [if addr > 0 && addr < 16 then
                                                replaceV regFile addr newData
                                                else regFile]
writeReg16 [regFile] _ _ [False] = [regFile]

readReg32 :: [Vector Int] -> [Register] -> [Int]
readReg32 [regFile] [addr] = [if addr >= 0 && addr < 32 then
                              atV regFile addr
                              else atV regFile 0]

readReg16 :: [Vector Int] -> [Register] -> [Int]
readReg16 [regFile] [addr] = [if addr >= 0 && addr < 16 then
                              atV regFile addr
                              else atV regFile 0]

regFile = vector $ replicate 32 0

-- Detectors
regFileNS _ [x] = x

writeRegScen0 = ((1,1,1,1),1, writeReg32)
writeRegScen1 = ((1,1,1,1),1, writeReg16)

writeRegSelScen 0 = (1, [writeRegScen0])
writeRegSelScen 1 = (1, [writeRegScen1])

d_wrRF = detector11SADF consume_rate regFileNS writeRegSelScen initial_state where
  consume_rate = 1
  initial_state = 0

readRegScen0 = ((1,1),1, readReg32)
readRegScen1 = ((1,1),1, readReg16)

readRegSelScen 0 = (1, [readRegScen0])
readRegSelScen 1 = (1, [readRegScen1])

d_rdRF1 = detector11SADF consume_rate regFileNS readRegSelScen initial_state where
  consume_rate = 1
  initial_state = 0

d_rdRF2 = detector11SADF consume_rate regFileNS readRegSelScen initial_state where
  consume_rate = 1
  initial_state = 0

---------------------------
-- System definition Risc-V
---------------------------
riscVsadf sIn = sFib where
  -- program counter process
  s_pc    = p_pcInit s_pcN
  s_pc4   = p_pcAdd4 s_pc
  s_pcTar = p_pcAddi s_pc s_imm
  s_pcN   = p_pcMux s_pc4 s_pcTar s_dir
  -- instruction data process
  s_instr = p_iMem s_pc
  -- instruction decoder process
  (s_rd,s_rs1,s_rs2,s_imm)                  = unzip4SY $ p_decoder1 s_instr
  (s_rfWR,s_aluOP,s_aluSRC,s_rfSRC,s_memWR) = unzip5SY $ p_decoder2 s_instr
  s_dir                                     = p_decoder3 s_instr s_alu0
  -- register file
  s_rfWRSDF         = syToSDF s_rfWR
  (s_rdSDF, s_tags) = sdfInsideSYIn s_rd
  s_rs1SDF          = syToSDF s_rs1
  s_rs2SDF          = syToSDF s_rs2
  s_rfDataSDF       = syToSDF s_rfData
  sdfControl        = syToSDF sIn
  f_1               = d_wrRF sdfControl
  f_2               = d_rdRF1 sdfControl
  f_3               = d_rdRF2 sdfControl
  v_rf              = delaySADF [regFile] v_rfN
  v_rfN             = kernel41SADF f_1 v_rf s_rdSDF s_rfDataSDF s_rfWRSDF
  s_reg1SDF         = kernel21SADF f_2 v_rf s_rs1SDF
  s_reg2SDF         = kernel21SADF f_3 v_rf s_rs2SDF
  s_reg1            = sdfInsideSYOut s_reg1SDF s_tags
  s_reg2            = sdfInsideSYOut s_reg2SDF s_tags
  s_rfData          = p_regMux s_aluRes s_pc4 s_memData s_rfSRC
  -- alu process
  s_aluRes = p_alu s_reg1 s_src2 s_aluOP
  s_alu0   = p_aluZero s_aluRes
  s_src2   = p_aluMux s_reg2 s_imm s_aluSRC
  -- data memory process
  v_dataMem    = p_dataMem v_dataMemNew
  v_dataMemNew = p_dataMemNew v_dataMem s_aluRes s_reg2 s_memWR
  s_memData    = p_dataMemRead v_dataMem s_aluRes
  -- fib test process
  sFib = p_testFib s_aluRes s_memWR

{-
test with the following command
ghci> takeS 72 $ riscVsadf sCtrl
{_,0,1,_,1,_,2,_,_,_,_,3,_,_,_,_,5,_,_,_,_,8,_,_,_,_,
13,_,_,_,_,21,_,_,_,_,34,_,_,_,_,55,_,_,_,_,89,_,_,_,_,
144,_,_,_,_,233,_,_,_,_,377,_,_,_,_,610,_,_,_,_,987}
-}

-- control signal that switches from RV32I to RV32E
sCtrl = signal [Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 1, Prst 1, Prst 1, Prst 1, Prst 1,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0,
                Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0, Prst 0]

---------------------------
-- Fig. 6 testing
---------------------------
func1 [x1, x2] = [x1 + x2]
func2 [x] = [x + 2, x * 2]

-- Fig. 6a implementation
system6a :: Num a => Signal (AbstExt a) -> Signal (AbstExt a)
system6a sIn = sOut
  where
    (x,t) = sdfInsideSYIn sIn
    sOut = sdfInsideSYOut y t
    y = actor11SDF 2 1 func1 x

-- Fig. 6c implementation
system6c :: Num a => Signal (AbstExt a) -> Signal (AbstExt a)
system6c sIn = sOut
  where
    (x,t) = sdfInsideSYIn sIn
    sOut = sdfInsideSYOut y t
    y = actor11SDF 1 2 func2 x

sTest2 = signal [Prst 1, Prst 2, Prst 3, Prst 4, Prst 5, Prst 6]