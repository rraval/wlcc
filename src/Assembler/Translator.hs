-- | Implements translation from 'Operation' to 'MipsWord' (i.e. binary mips representation)
module Assembler.Translator(translate) where

import Assembler.Data
import Data.Bits((.|.), shiftL)
import qualified Data.Map as M

-- | Generate a R-format instruction. See 'translate'.
genRegister :: Funct -> Destination -> Source -> Tource -> MipsWord
genRegister o d s t =
    let s' = (fromIntegral s :: MipsWord) `shiftL` 21
        t' = (fromIntegral t :: MipsWord) `shiftL` 16
        d' = (fromIntegral d :: MipsWord) `shiftL` 11
        o' = fromIntegral o  :: MipsWord
    in s' .|. t' .|. d' .|. o'

-- | Generate an I-format instruction. See 'translate'.
genImmediate :: Opcode -> Source -> Tource -> Offset -> MipsWord
genImmediate op s t off =
    let op' = (fromIntegral op :: MipsWord) `shiftL` 26
        s' = (fromIntegral s :: MipsWord) `shiftL` 21
        t' = (fromIntegral t :: MipsWord) `shiftL` 16
        off' = fromIntegral off :: MipsWord
    in op' .|. s' .|. t' .|. off'

-- | Looks up a label and returns a relative offset understood by 'Beq' and its like, or generates
--   an error if the label has never been defined.
genOffset :: Metadata -> Label -> Integer -> Offset
genOffset m l c =
    case M.lookup l $ labelTable m of
        Nothing -> error $ "Unknown label: " ++ l
        Just o  ->
            let off = o - c - 1 in
                if off > 0xffff
                then error "Offset between word offset and label cannot fit"
                else fromIntegral off :: Offset

-- | Generate a corresponding bit pattern for every 'Operation'.
translate :: Metadata -> Operation -> MipsWord
translate m (Add d s t)     = genRegister 0x20 d s t
translate m (Beq s t o)     = genImmediate 0x4 s t o
translate m (BeqL s t l c)  = translate g $ Beq s t $ genOffset m l c
translate m (Bne s t o)     = genImmediate 0x5 s t o
translate m (BneL s t l c)  = translate g $ Bne s t $ genOffset m l c
translate m (Div s t)       = genRegister 0x1a 0 s t
translate m (Divu s t)      = genRegister 0x1b 0 s t
translate m (Jalr s)        = genRegister 0x8 0 s 0
translate m (Jr s)          = genRegister 0x9 0 s 0
translate m (Lis d)         = genRegister 0x14 d 0 0
translate m (Lw t o s)      = genImmediate 0x23 s t o
translate m (Mfhi d)        = genRegister 0x10 d 0 0
translate m (Mflo d)        = genRegister 0x12 d 0 0
translate m (Mult s t)      = genRegister 0x18 0 s t
translate m (Multu s t)     = genRegister 0x19 0 s t
translate m (Slt d s t)     = genRegister 0x2a d s t
translate m (Sltu d s t)    = genRegister 0x2b d s t
translate m (Sub d s t)     = genRegister 0x22 d s t
translate m (Sw t o s)      = genImmediate 0x2b s t o
translate m (Word w)        = w
