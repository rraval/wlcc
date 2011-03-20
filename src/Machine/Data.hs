{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Holds all the data types used by the WL MIPS machine.
module Machine.Data (
    -- * Binary File Format
    -- $binary-intro

    -- ** I-format
    -- $iformat

    -- ** R-format
    -- $rformat
    Operation(..),
    -- * Restriced Type Constructors
    makeOffset,
    makeRegister,
    -- * Register Types
    Destination,
    Source,
    Tource) where

import Data.Binary(Binary(..))
import Data.Bits(Bits, (.&.), shiftL, shiftR)
import Data.Char(chr)
import qualified Data.Map as M
import Data.Word(Word8, Word16, Word32)

-- | Data type that represents all possible instructions supported by the machine.
--
--   Conventions:
--
--      [@$d@]      'Destination' register.
--
--      [@$s@]      'Source' register.
--
--      [@$t@]      'Tource' (secondary source) register.
--
--      [@i@]       Offset. Used for @I-format@ instructions.
--
--      [/HI/]      32-bit internal register.
--
--      [/LO/]      32-bit internal register.
--
--      [/PC/]      Program counter. Holds the offset to next instruction and is incremented
--                  after each instruction execution (word-indexed).
--
--      [/MEM/]     Abstraction to treat memory as an array. This is byte indexed but the index
--                  must lie on a word boundary (i.e. a multiple of 4).
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

-- $binary-intro
-- Serialization and deserialization routines for 'Operation' are provided by an instance of the
-- 'Binary' typeclass. Essentially, this defines the binary file format for executables.
--
-- Every 'Operation' is serialized into a single 'Word32' (hereafter referred to as "word"). The
-- six higher order bits of each word is called the @op@ field, which signifies the type of the
-- encoded instruction.
--
-- If the @op@ field is non-zero, then the encoded 'Operation' type is in the @I-format@. If @op@
-- is zero, then it is in the @R-format@.

-- $iformat
-- The immediate format instructions encode a 16-bit offset. Thus, only
-- 'Sw', 'Lw', 'Beq', and 'Bne' use this kind of encoding. The format is:
--
-- @
--     oooo ooss ssst tttt iiii iiii iiii iiii
--     ^- Bit 31                      Bit 0 -^
-- @
--
-- Here, the @o@ bits are the @op@ field, the @s@ and @t@ bits encode two registers ('Source'
-- and 'Tource' respectively), while the @i@ bits represent the 16 bit offsets for the
-- respective instructions.
--
-- The @op@ field also distinguishes the encoded 'Operation':
--
--     * @1000 11@ is a 'Lw'
--
--     * @1010 11@ is a 'Sw'
--
--     * @0001 00@ is a 'Beq'
--
--     * @0001 01@ is a 'Bne'

-- $rformat
-- Register format instructions encode 3 registers. The @op@ field is
-- zero and the 6 lower order bits define the @funct@ field. The format is:
--
-- @
--     0000 00ss ssst tttt dddd d000 00ff ffff
--     ^- Bit 31                      Bit 0 -^
-- @
--
-- The @s@, @t@, and @d@ bits encode the 'Source', 'Tource', and 'Destination' bits of each
-- instruction. It should be noted that some instructions might not use all registers, for
-- example, 'Mult' only uses 'Source' and 'Tource', while 'Mfhi' only uses 'Destination'.
-- Unused registers are encoded as zeros.
--
-- The @f@ bits define the @funct@ field, which distinguishes the instruction being encoded:
--
--     * @10 0000@ is 'Add'
--
--     * @10 0010@ is 'Sub'
--
--     * @01 1000@ is 'Mult'
--
--     * @01 1001@ is 'Multu'
--
--     * @01 1010@ is 'Div'
--
--     * @01 1011@ is 'Divu'
--
--     * @01 0000@ is 'Mfhi'
--
--     * @01 0010@ is 'Mflo'
--
--     * @01 0100@ is 'Lis'
--
--     * @10 1010@ is 'Slt'
--
--     * @10 1011@ is 'Sltu'
--
--     * @00 1000@ is 'Jr'
--
--     * @00 1001@ is 'Jalr'
instance Binary Operation where
    put = undefined
    get = undefined

type Register = Word8

-- | Provides range checking for the creation of 'Register'. This is the /only/ way to construct the
--   'Register' type.
makeRegister :: (Integral a) => a -> Register
makeRegister r
    | r < 0 || r > 31 = error $ "No such register: $" ++ show r
    | otherwise = fromIntegral r

type Offset = Word16

-- | Provides range checking for the creation of 'Offset'. This is the /only/ way to construct the
--   'Offset' type. Seen as @i@ in documentation.
makeOffset :: (Integral a) => a -> Offset
makeOffset o
    | o < fromIntegral (minBound :: Offset)
        || o > fromIntegral (maxBound :: Offset) = error $ "Offset out of range: " ++ show o
    | otherwise = fromIntegral o

type Destination = Register -- ^ Register specifying destination. Seen as @$d@ in documentation.
type Source = Register      -- ^ Register specifying first source. Seen as @$s@ in documentation.
type Tource = Register      -- ^ Register specifying second source. Seen as @$t@ in documentation.
