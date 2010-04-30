module Assembler.Translator(translate) where

import Assembler.Data
import Data.Bits((.|.), shiftL)
import qualified Data.Map as M

-- | Generate a R-format instruction.
genRegister :: Funct -> Destination -> Source -> Tource -> MipsWord
genRegister o d s t = let   s' = (fromIntegral s :: MipsWord) `shiftL` 21
                            t' = (fromIntegral t :: MipsWord) `shiftL` 16
                            d' = (fromIntegral d :: MipsWord) `shiftL` 11
                            o' = fromIntegral o  :: MipsWord
                      in s' .|. t' .|. d' .|. o'

-- | Generate an I-format instruction.
genImmediate :: Opcode -> Source -> Tource -> Offset -> MipsWord
genImmediate op s t off = let   op'  = (fromIntegral op  :: MipsWord) `shiftL` 26
                                s'   = (fromIntegral s   :: MipsWord) `shiftL` 21
                                t'   = (fromIntegral t   :: MipsWord) `shiftL` 16
                                off' = fromIntegral off  :: MipsWord
                          in op' .|. s' .|. t' .|. off'

-- | Looks up a label and returns a relative offset understood by 'Beq' and its like, or generates
--   an error if the label has never been defined.
genOffset :: Generation -> Label -> Offset
genOffset g l   = case M.lookup l $ labelTable g of
                    Nothing -> error $ "Unknown label: " ++ l
                    Just o  -> let off = o - wordOffset g - 1 in
                                   if off > 0xffff
                                   then error "Offset between word offset and label cannot fit"
                                   else fromIntegral off :: Offset

-- | Generate a corresponding instruction for every 'Operation'.
translate :: Generation -> Operation -> MipsWord
translate g (Add d s t)     = genRegister 0x20 d s t
translate g (Beq s t o)     = genImmediate 0x4 s t o
translate g (BeqL s t l)    = translate g $ Beq s t $ genOffset g l
translate g (Bne s t o)     = genImmediate 0x5 s t o
translate g (BneL s t l)    = translate g $ Bne s t $ genOffset g l
translate g (Div s t)       = genRegister 0x1a 0 s t
translate g (Divu s t)      = genRegister 0x1b 0 s t
translate g (Jalr s)        = genRegister 0x8 0 s 0
translate g (Jr s)          = genRegister 0x9 0 s 0
translate g (Lis d)         = genRegister 0x14 d 0 0
translate g (Lw t o s)      = genImmediate 0x23 s t o
translate g (Mfhi d)        = genRegister 0x10 d 0 0
translate g (Mflo d)        = genRegister 0x12 d 0 0
translate g (Mult s t)      = genRegister 0x18 0 s t
translate g (Multu s t)     = genRegister 0x19 0 s t
translate g (Slt d s t)     = genRegister 0x2a d s t
translate g (Sltu d s t)    = genRegister 0x2b d s t
translate g (Sub d s t)     = genRegister 0x22 d s t
translate g (Sw t o s)      = genImmediate 0x2b s t o
translate g (Word w)        = w
