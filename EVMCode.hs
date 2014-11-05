{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module EVMCode where

import Prelude hiding (EQ, GT, LT)

import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import Data.List (mapAccumL)
import Data.Word
import Numeric (showHex)

data LargeWord = LargeWord
                 { wordSize :: Int         	-- max. 32
                 , wordBytes :: [Word8] } 	-- wordSize bytes
               deriving (Show, Eq, Ord)

makeWord :: (Integral i) => i -> LargeWord
makeWord i = LargeWord (length bs) bs
    where bs = integer2bytes i

          integer2bytes :: (Integral i) => i -> [Word8]
          integer2bytes = i2bs []

          i2bs ws i =
              case i `divMod` 256 of
                (0, mod) -> fromIntegral mod : ws
                (j, mod) -> i2bs (fromIntegral mod : ws) j

makeNWord :: (Integral i) => Int -> i -> LargeWord
makeNWord sz i = LargeWord sz paddedBytes
    where paddedBytes = 
              case makeWord (toInteger i `mod` 2^(sz*8)) of
                LargeWord size bytes -> 
                    (replicate (sz - size) (fromInteger 0)) ++ bytes

makeWord32 = makeNWord 4
makeWord16 = makeNWord 2

numValue :: (Num a) => LargeWord -> a
numValue = fromInteger . bytes2I 0 . wordBytes
    where bytes2I i (w8:bytes) = bytes2I (i * 256 + toInteger w8) bytes
          bytes2I i [] = i


data EVMOpcode =
    -- 0x00
    STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD |
    ADDMOD | MULMOD | EXP | SIGNEXTEND |
    -- 0x10
    LT | GT | SLT | SGT | EQ | ISZERO | AND | OR |
    XOR | NOT | BYTE |
    -- 0x20
    SHA3 |
    -- 0x30
    ADDRESS | BALANCE | ORIGIN | CALLER |
    CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY |
    CODESIZE | CODECOPY | GASPRICE | EXTCODESIZE |
    EXTCODECOPY |
    -- 0x40
    PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT |
    -- 0x50
    POP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI |
    PC | MSIZE | GAS | JUMPDEST |
    -- 0xf0
    CREATE | CALL | RETURN | CALLCODE |
    -- 0xff
    SUICIDE 
  deriving (Show, Eq, Ord, A.Ix)


type Label = String
data EVMInstr = EVMSimple EVMOpcode
              | EVMPush LargeWord
              | EVMDup Int
              | EVMSwap Int
              | EVMLog Int
              | EXTComment String
              | EXTLabel Label
              | EXTLabelAddr Label
                deriving (Eq)

instance Show EVMInstr where
  show (EVMSimple op) = show op
  show (EVMPush word) = "PUSH" ++ show (wordSize word) ++
                        " " ++ show (wordBytes word)
  show (EVMDup n) = "DUP" ++ show n
  show (EVMSwap n) = "SWAP" ++ show n
  show (EVMLog n) = "LOG" ++ show n
  show (EXTComment s) = ";; " ++ s
  show (EXTLabel l) = l ++ ":"
  show (EXTLabelAddr l) = "[" ++ l ++ "]"

type EVMCode = [EVMInstr]

instance Show EVMCode where
  show code = foldr (\ i s -> i ++ '\n':s) "" (map show code)

showPos :: [(Int,EVMInstr)] -> String
showPos code = concat $ map showInstr code
    where showInstr (pos, EXTLabel l) = l ++ ":\n"
          showInstr (pos, EXTComment s) = ";; " ++ s ++ "\n"
          showInstr (pos, instr) = shows pos $ ":\t" ++ shows instr "\n"
               

{-
showPos :: EVMCode -> String
showPos code = concat strings
    where (_, strings) = mapAccumL (\ pos instr -> 
                                        ( pos + instrSize 0 instr
                                        , instrAt pos instr ))
                         0 code
          instrAt pos (EXTLabel l) = l ++ ":\n"
          instrAt pos (EXTComment s) = ";; " ++ s ++ "\n"
          instrAt pos instr = shows pos $ ":\t" ++ shows instr "\n"
-}

instrSize :: Int -> EVMInstr -> Int
instrSize _ (EVMPush lw) = 1 + wordSize lw
instrSize _ (EXTComment _) = 0
instrSize labelSize (EXTLabel _) = instrSize labelSize (EVMSimple JUMPDEST)
instrSize labelSize (EXTLabelAddr _) = labelSize + 1 -- 1 added for PUSH
instrSize _ _ = 1

codeSize :: Int -> EVMCode -> Int
codeSize labelSize = sum . map (instrSize labelSize)


instr2bytes :: EVMInstr -> [Word8]
instr2bytes (EVMPush lw) = (fromIntegral (0x60 - 1 + wordSize lw))
                           : wordBytes lw
instr2bytes (EVMDup n)   = [fromIntegral (0x80 - 1 + n)]
instr2bytes (EVMSwap n)  = [fromIntegral (0x90 - 1 + n)]
instr2bytes (EVMLog n)   = [fromIntegral (0xa0 + n)] -- operand starts from 0
instr2bytes (EVMSimple op) = [fromIntegral (opcode2byteMap A.! op)]

instr2bytes (EXTComment _) = []
instr2bytes (EXTLabel _) = instr2bytes (EVMSimple JUMPDEST)
instr2bytes (EXTLabelAddr l) = error "EXTLabelAddr has undefined byte representation!"

code2bytes :: EVMCode -> [Word8]
code2bytes = foldr (++) [] . map instr2bytes


code2hexString :: EVMCode -> String
code2hexString = concat . (map instr2hex) . code2bytes
  where instr2hex opcode =
          case showHex opcode "" of
            [digit] -> ['0', digit]
            digits -> digits

opcode2byteMap :: A.Array EVMOpcode Word8
opcode2byteMap = A.array (STOP, SUICIDE) opcodeBytePairs

byte2opcodeMap :: A.Array Word8 EVMOpcode
byte2opcodeMap = A.array (0, 0xff) byteOpcodePairs


opcodeBytePairs :: [(EVMOpcode, Word8)]
opcodeBytePairs = map (\ (b,o) -> (o,b) ) byteOpcodePairs

byteOpcodePairs :: [(Word8, EVMOpcode)]
byteOpcodePairs =
  zip [fromIntegral 0x00 ..]
  [STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD,
   ADDMOD, MULMOD, EXP, SIGNEXTEND ]
  ++
  zip [0x10 ..]
  [LT, GT, SLT, SGT, EQ, ISZERO, AND, OR,
   XOR, NOT, BYTE ]
  ++
  [(0x20, SHA3)]
  ++
  zip [0x30 ..]
  [ADDRESS, BALANCE, ORIGIN, CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE,
   CALLDATACOPY, CODESIZE, CODECOPY, GASPRICE, EXTCODESIZE, EXTCODECOPY]
  ++
  zip [0x40 ..]
  [PREVHASH, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT]
  ++
  zip [0x50 ..]
  [POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE, JUMP, JUMPI,
   PC, MSIZE, GAS, JUMPDEST]
  ++
  zip [0xf0 ..]
  [CREATE, CALL, RETURN, CALLCODE]
  ++
  [(0xff, SUICIDE)]

  
