{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Holds all the data types used in the assembler.
module Assembler.Data where

import Data.Bits(Bits, (.&.), shiftL, shiftR)
import Data.Char(chr)
import qualified Data.Map as M
import Data.Word(Word8, Word16, Word32)

-- | Data type that represents all possible instructions supported by the assembler.
data Operation  = Add   Destination Source  Tource        -- ^ [@add $d, $s, $t@]
                                                          --      Addition. @$d = $s + $t@.
                | Beq   Source      Tource  Offset        -- ^ [@beq $s, $t, i@]
                                                          --      Branch on Equal. /i/ must be 16
                                                          --      bits or less.
                                                          --
                                                          --      @
                                                          --          if   $s = $t
                                                          --          then /PC/ = /PC/ + i
                                                          --      @
                | BeqL  Source      Tource  Label Integer -- ^ [@beq $s, $t, /label/@]
                                                          --      Branch on Equal. Computes offset
                                                          --      from current position to given
                                                          --      label, which must be defined
                                                          --      elsewhere. See 'Beq'.
                                                          --
                                                          --      The last 'Word32' holds the current
                                                          --      position.
                | Bne   Source      Tource  Offset        -- ^ [@bne $s, $t, i@]
                                                          --      Branch on Not Equal. /i/ must be 16
                                                          --      bits or less.
                                                          --
                                                          --      @
                                                          --          if   $s /= $t
                                                          --          then /PC/ = /PC/ + i
                                                          --      @
                | BneL  Source      Tource  Label Integer -- ^ [@bne $s, $t, /label/@]
                                                          --      Branch on Not Equal. Like 'Bne', but
                                                          --      accepts a label instead of offset.
                                                          --      See 'BeqL'
                                                          --
                                                          --      The last 'Word32' holds the current
                                                          --      position.
                | Div   Source      Tource                -- ^ [@div $s, $t@]
                                                          --      Signed Division.
                                                          --
                                                          --      @
                                                          --          /HI/ = $s \`mod\` $t
                                                          --          /LO/ = $s / $t
                                                          --      @
                | Divu  Source      Tource                -- ^ [@divu $s, $t@]
                                                          --      Unsigned Division. See 'Div'
                | Jalr  Source                            -- ^ [@jalr $s@]
                                                          --      Jump and Link Register.
                                                          --
                                                          --      @
                                                          --          $31 = /PC/
                                                          --          /PC/ = $s
                                                          --      @
                | Jr    Source                            -- ^ [@jr $s@]
                                                          --      Jump Register. @/PC/ = $s@
                | Lis   Destination                       -- ^ [@lis $d@]
                                                          --      Load Immediate and Skip.
                                                          --
                                                          --      @
                                                          --          $d = MEM[/PC/]
                                                          --          /PC/ = /PC/ + 4
                                                          --      @
                | Lw    Tource      Offset  Source        -- ^ [@lw $t, i($s)@]
                                                          --      Load Word. @$t = MEM[$s + i]@
                | Mfhi  Destination                       -- ^ [@mfhi $d@]
                                                          --      Move from /HI/. @$d = /HI/@
                | Mflo  Destination                       -- ^ [@mflo $d@]
                                                          --      Move from /LO/. @$d = /LO/@
                | Mult  Source      Tource                -- ^ [@mult $s, $t@]
                                                          --      Signed Multiplication.
                                                          --      @/HI:LO/ = $s * $t@.
                | Multu Source      Tource                -- ^ [@multu $s, $t@]
                                                          --      Unsigned Multiplication.
                                                          --      @/HI:LO/ = $s * $t@.
                | Slt   Destination Source  Tource        -- ^ [@slt $d, $s, $t@]
                                                          --      Set Less Than.
                                                          --
                                                          --      @
                                                          --          if   $s < $t
                                                          --          then $d = 1
                                                          --          else $d = 0
                                                          --      @
                | Sltu  Destination Source  Tource        -- ^ [@sltu $d, $s, $t@]
                                                          --      Unsigned Set Less Than. See 'Slt'.
                | Sub   Destination Source  Tource        -- ^ [@sub $d, $s, $t@]
                                                          --      Subtraction. @$d = $s - $t@.
                | Sw    Tource      Offset  Source        -- ^ [@sw $t, i($s)@]
                                                          --      Store Word. @MEM[$s + i] = $t@
                | Word  MipsWord                          -- ^ [@.word i@]
                                                          --      Meta operation. The 32 bit number
                                                          --      /i/ will appear literally in the
                                                          --      assembler output.

-- | Holds state data about the current instruction being assembled.
data Generation = Generation {
                    labelTable :: M.Map String Integer, -- ^ 'M.Map' holding all defined labels and
                                                        --   their respective values.
                    wordOffset :: Integer               -- ^ Current instruction offset (i.e. how
                                                        --   many instructions have already been
                                                        --   assembled).
                  }

-- | Type used for every assembled instruction.
newtype MipsWord = MipsWord Word32
    deriving (Bits, Bounded, Enum, Eq, Integral, Num, Ord, Real)

instance Show MipsWord where
    show = map chr . splitBytes
      where splitBytes w    = [toByte 24 w, toByte 16 w, toByte 8 w, toByte 0 w]
            toByte n w      = fromIntegral $ (w .&. (0xff `shiftL` n)) `shiftR` n :: Int
    showList = showString . concat . map show

type Opcode = Word8         -- ^ Specifies the top 6 bits of an instruction (the /opcode/ field
                            --   of a MIPS instruction.
type Funct = Word8          -- ^ Specifies the lower 6 bits of an instruction (the /funct/ field
                            --   of a MIPS R-format instruction.
type Register = Word8       -- ^ Type used to specify registers.
type Destination = Register -- ^ Register specifying destination. Seen as @$d@ in documentation.
                            --   Only uses 5 bits.
type Source = Register      -- ^ Register specifying first source. Seen as @$s@ in documentation.
                            --   Only uses 5 bits.
type Tource = Register      -- ^ Register specifying second source. Seen as @$t@ in documentation.
                            --   Only uses 5 bits.
type Offset = Word16        -- ^ Constants used in immediate instructions (see 'genImmediate').
                            --   Seen as @i@ in documentation. Uses all 16 bits.
type Label = String         -- ^ Refers to a named location in the file. See as @/label/@ in
                            --   documentation.
