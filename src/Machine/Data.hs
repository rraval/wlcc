{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Holds all the data types used by the WL MIPS machine.
module Machine.Data where

import Data.Bits(Bits, (.&.), shiftL, shiftR)
import Data.Char(chr)
import qualified Data.Map as M
import Data.Word(Word8, Word16, Word32)

-- | Data type that represents all possible instructions supported by the machine.
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
                | Bne   Source      Tource  Offset        -- ^ [@bne $s, $t, i@]
                                                          --      Branch on Not Equal. /i/ must be 16
                                                          --      bits or less.
                                                          --
                                                          --      @
                                                          --          if   $s /= $t
                                                          --          then /PC/ = /PC/ + i
                                                          --      @
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
                | Literal  Word32                         -- ^ [@.word i@]
                                                          --      Meta operation. The 32 bit number
                                                          --      /i/ will appear literally in the
                                                          --      assembler output.
                deriving (Eq, Show)

-- | Type used to specify registers.
type Register = Word8

-- | Provides range checking for the creation of 'Register'. This is the /only/ way to construct the
--   'Register' type.
makeRegister r
    | r < (minBound :: Register) || r > (maxBound :: Register) = error $ "No such register: $" ++ show r
    | otherwise = Register r

-- | Constants used in immediate instructions (see 'genImmediate').
--   Seen as @i@ in documentation. Uses all 16 bits.
type Offset = Word16

-- | Provides range checking for the creation of 'Offset'. This is the /only/ way to construct the
--   'Offset' type.
makeOffset o
    | o < (minBound :: Offset) || r > (maxBound :: Register) = error $ "Offset out of range: " ++ show o
    | otherwise = Offset o

type Destination = Register -- ^ Register specifying destination. Seen as @$d@ in documentation.
                            --   Only uses 5 bits.
type Source = Register      -- ^ Register specifying first source. Seen as @$s@ in documentation.
                            --   Only uses 5 bits.
type Tource = Register      -- ^ Register specifying second source. Seen as @$t@ in documentation.
                            --   Only uses 5 bits.
