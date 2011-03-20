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
    Offset,
    makeRegister,
    Register,
    -- * Register Types
    Destination,
    Source,
    Tource) where

import Control.Monad(liftM, liftM2, liftM3)
import Data.Binary(Binary(..), Get, put)
import Data.Bits((.&.), (.|.), shiftL, shiftR)
import Data.Word(Word8, Word16, Word32)
import Test.QuickCheck(Arbitrary(..), arbitraryBoundedIntegral, oneof)
import Text.Printf(printf)

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

instance Arbitrary Operation where
    arbitrary = oneof
        [ liftM3 Add arbitrary arbitrary arbitrary
        , liftM3 Beq arbitrary arbitrary arbitrary
        , liftM3 Bne arbitrary arbitrary arbitrary
        , liftM2 Div arbitrary arbitrary
        , liftM2 Divu arbitrary arbitrary
        , liftM Jalr arbitrary
        , liftM Jr arbitrary
        , liftM Lis arbitrary
        , liftM3 Lw arbitrary arbitrary arbitrary
        , liftM Mfhi arbitrary
        , liftM Mflo arbitrary
        , liftM2 Mult arbitrary arbitrary
        , liftM2 Multu arbitrary arbitrary
        , liftM3 Slt arbitrary arbitrary arbitrary
        , liftM3 Sltu arbitrary arbitrary arbitrary
        , liftM3 Sub arbitrary arbitrary arbitrary
        , liftM3 Sw arbitrary arbitrary arbitrary
        , liftM Literal arbitrary
        ]

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
    put opn = case opn of
        (Add d s t)     -> putR d s t 0x20
        (Beq s t i)     -> putI 0x4 s t i
        (Bne s t i)     -> putI 0x5 s t i
        (Div s t)       -> putR 0 s t 0x1a
        (Divu s t)      -> putR 0 s t 0x1b
        (Jalr s)        -> putR 0 s 0 0x9
        (Jr s)          -> putR 0 s 0 0x8
        (Lis d)         -> putR d 0 0 0x14
        (Lw t i s)      -> putI 0x23 s t i
        (Mfhi d)        -> putR d 0 0 0x10
        (Mflo d)        -> putR d 0 0 0x12
        (Mult s t)      -> putR 0 s t 0x18
        (Multu s t)     -> putR 0 s t 0x19
        (Slt d s t)     -> putR d s t 0x2a
        (Sltu d s t)    -> putR d s t 0x2b
        (Sub d s t)     -> putR d s t 0x22
        (Sw t i s)      -> putI 0x2b s t i
        (Literal w)     -> put w
      where
        putI op s t i = do
            let op' = (fromIntegral op :: Word32) `shiftL` 26
                s' = (fromIntegral s :: Word32) `shiftL` 21
                t' = (fromIntegral t :: Word32) `shiftL` 16
                i' = fromIntegral i :: Word32
            put $ op' .|. s' .|. t' .|. i'

        putR d s t funct = do
            let s' = (fromIntegral s :: Word32) `shiftL` 21
                t' = (fromIntegral t :: Word32) `shiftL` 16
                d' = (fromIntegral d :: Word32) `shiftL` 11
                funct' = fromIntegral funct :: Word32
            put $ s' .|. t' .|. d' .|. funct'

    get = do
        w <- get :: Get Word32
        return $ case w .&. 0xfc000000 of
            0x8c000000  -> applyS w $ applyI w $ applyT w Lw
            0xac000000  -> applyS w $ applyI w $ applyT w Sw
            0x10000000  -> applyI w $ applyT w $ applyS w Beq
            0x14000000  -> applyI w $ applyT w $ applyS w Bne
            0           -> case w .&. 0x3f of   -- R-format
                0x20    -> applyT w $ applyS w $ applyD w Add
                0x22    -> applyT w $ applyS w $ applyD w Sub
                0x18    -> applyT w $ applyS w Mult
                0x19    -> applyT w $ applyS w Multu
                0x1a    -> applyT w $ applyS w Div
                0x1b    -> applyT w $ applyS w Divu
                0x10    -> applyD w Mfhi
                0x12    -> applyD w Mflo
                0x14    -> applyD w Lis
                0x2a    -> applyT w $ applyS w $ applyD w Slt
                0x2b    -> applyT w $ applyS w $ applyD w Sltu
                0x08    -> applyS w Jr
                0x09    -> applyS w Jalr
                _       -> error $ printf "Bad R-format instruction: %08x" w
            _           -> error $ printf "Bad instruction: %08x" w
      where
        -- these work for both I-format and R-format
        applyS w cns = cns $ makeRegister $ (w .&. 0x03e00000) `shiftR` 21
        applyT w cns = cns $ makeRegister $ (w .&. 0x001f0000) `shiftR` 16
        -- R-format only
        applyD w cns = cns $ makeRegister $ (w .&. 0x0000f800) `shiftR` 11
        -- I-format only
        applyI w cns = cns $ makeOffset $ w .&. 0x0000ffff

newtype Register = Register Word8
    deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded Register where
    minBound = 0
    maxBound = 31

instance Show Register where
    show (Register r) = show r

instance Arbitrary Register where
    arbitrary = arbitraryBoundedIntegral

-- | Provides range checking for the creation of 'Register'. This is the /only/ way to construct the
--   'Register' type.
makeRegister :: (Integral a) => a -> Register
makeRegister r
    | r < fromIntegral (minBound :: Register)
        || r > fromIntegral (maxBound :: Register) = error $ "No such register: $" ++ show r
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
