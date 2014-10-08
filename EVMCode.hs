{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module EVMCode where

import Prelude hiding (EQ, GT, LT)
import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Array.Unboxed as A

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

makeWord32 :: (Integral i) => i -> LargeWord
makeWord32 i = LargeWord 4 paddedBytes
  where paddedBytes = case makeWord (i `mod` 2^32) of
          LargeWord size bytes -> (replicate (4-size) (fromInteger 0)) ++ bytes


data EVMOpcode =
    -- 0x00
    STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD |
    EXP | NEG | LT | GT | SLT | SGT | EQ | NOT |
    -- 0x10
    AND | OR | XOR | BYTE | ADDMOD | MULMOD |
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
    POP |
    -- 0x53
    MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI |
    PC | MSIZE | GAS | JUMPDEST |
    -- 0x60
    -- PUSH | -- LargeWord |
    -- 0x80
    -- DUP | -- Int |
    -- 0x90
    -- SWAP | -- Int |
    -- 0xf0
    CREATE | CALL | RETURN | POST | CALLSTATELESS |
    -- 0xff
    SUICIDE 
  deriving (Show, Eq, Ord, A.Ix)

-- instance A.Ix EVMOpcode where
  

data EVMInstr a = EVMSimple EVMOpcode
                | EVMPush LargeWord
                | EVMDup Int
                | EVMSwap Int
                | EXTComment String
                | EXTFuncAddr a
                deriving (Eq)

instance Show a => Show (EVMInstr a) where
  show (EVMSimple op) = show op
  show (EVMPush word) = "PUSH" ++ show (wordSize word) ++
                        " " ++ show (wordBytes word)
  show (EVMDup n) = "DUP" ++ show n
  show (EVMSwap n) = "SWAP" ++ show n
  show (EXTComment s) = ";; " ++ s
  show (EXTFuncAddr a) = "ADDROF " ++ show a

type EVMCode a = [EVMInstr a]

instance Show a => Show (EVMCode a) where
  show code = foldr (\ i s -> i ++ '\n':s) "" (map show code)

instr2bytes :: EVMInstr a -> [Word8]
instr2bytes (EVMPush lw) = (fromIntegral (0x60 - 1 + wordSize lw))
                           : wordBytes lw
instr2bytes (EVMDup n)   = [fromIntegral (0x80 - 1 + n)]
instr2bytes (EVMSwap n)  = [fromIntegral (0x90 - 1 + n)]
instr2bytes (EVMSimple op) = [fromIntegral (opcode2byteMap A.! op)]

instr2bytes (EXTComment s) = []
instr2bytes (EXTFuncAddr a) = instr2bytes (EVMPush zero4)
                              where zero4 = LargeWord 4
                                            (replicate 4 (fromIntegral 0))

code2bytes :: EVMCode a -> [Word8]
code2bytes = foldr (++) [] . map instr2bytes

codeLength :: EVMCode a -> Int
codeLength = length . code2bytes 

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
   EXP, NEG, LT, GT, SLT, SGT, EQ, NOT ]
  ++
  zip [0x10 ..]
  [AND, OR, XOR, BYTE, ADDMOD, MULMOD ] 
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
  [(0x50, POP)]
  ++
  zip [0x53 ..]
  [MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE, JUMP, JUMPI, PC, MSIZE, GAS, JUMPDEST]
  ++
  zip [0xf0 ..]
  [CREATE, CALL, RETURN, POST, CALLSTATELESS]
  ++
  [(0xff, SUICIDE)]

  
