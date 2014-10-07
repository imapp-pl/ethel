module EVMCode where

import Prelude hiding (EQ, GT, LT)
import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Array.Unboxed as A

data LargeWord = LargeWord
                 { wordSize :: Int         	-- max. 32
                 , wordBytes :: [Word8] } 	-- wordSize bytes
               deriving (Show, Eq, Ord)


makeWord :: Integer -> LargeWord
makeWord i = LargeWord (length bs) bs
  where bs = integer2bytes i

        integer2bytes :: Integer -> [Word8]
        integer2bytes = i2bs []

        i2bs ws i =
          case i `divMod` 256 of
            (0, mod) -> fromInteger mod : ws
            (j, mod) -> i2bs (fromInteger mod : ws) j


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
    PC | MSIZE | GAS |
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
  

data EVMInstr = EVMSimple EVMOpcode
              | EVMPush LargeWord
              | EVMDup Int
              | EVMSwap Int
              deriving (Show, Eq)

type EVMCode = [EVMInstr]


instr2bytes :: EVMInstr -> [Word8]
instr2bytes (EVMPush lw) = (fromIntegral (0x60 - 1 + wordSize lw))
                           : wordBytes lw
instr2bytes (EVMDup n)   = [fromIntegral (0x80 - 1 + n)]
instr2bytes (EVMSwap n)  = [fromIntegral (0x90 - 1 + n)]
instr2bytes (EVMSimple op) = [fromIntegral (opcode2byteMap A.! op)]

code2bytes :: EVMCode -> [Word8]
code2bytes = foldr (++) [] . map instr2bytes


opcode2byteMap :: A.Array EVMOpcode Word8
opcode2byteMap = A.array (STOP, SUICIDE) opcodeBytePairs

byte2OpcodeMap :: A.Array Word8 EVMOpcode
byte2OpcodeMap = A.array (0, 0xff) byteOpcodePairs


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
  [MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE, JUMP, JUMPI, PC, MSIZE, GAS]
  ++
  zip [0xf0 ..]
  [CREATE, CALL, RETURN, POST, CALLSTATELESS]
  ++
  [(0xff, SUICIDE)]
  

  
