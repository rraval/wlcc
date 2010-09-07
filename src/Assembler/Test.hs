module Assembler.Tests(main) where

import Control.Monad(liftM, liftM2, liftM3)
import Data.Word(Word16, Word32)
import Test.QuickCheck

import Assembler.Data
import Assembler.Parser

instance Arbitrary Register where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary Word16 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary Word32 where
  arbitrary = arbitraryBoundedIntegral

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
              , liftM Word arbitrary
              ]

-- for labels
{-
newtype LabelChar = LabelChar Char
instance Show LabelChar where
  show (LabelChar c) = c:[]
  showList = showString . foldr extract []
    where extract (LabelChar c) cs = c : cs
instance Arbitrary LabelChar where
  arbitrary = elements $ map LabelChar $ ['a'..'z'] ++ ['A'..'Z']
-}

newtype Whitespace = Whitespace Char
instance Arbitrary Whitespace where
  arbitrary = elements $ map Whitespace $ [' ', '\t', '\f', '\v', '\n']
instance Show Whitespace where
  show (Whitespace c) = c:[]
  showList = showString . foldr extract []
    where extract (Whitespace c) cs = c : cs

data ProgramLine = ProgramLine
               { expected   :: Operation
               , source     :: String
               } deriving (Show)
instance Arbitrary ProgramLine where
  arbitrary = do
    op <- (arbitrary :: Gen Operation)
    case op of
      (Add d s t)   -> register3 op "add" d s t
      (Beq s t o)   -> offset op "beq" s t o
      (Bne s t o)   -> offset op "bne" s t o
      (Div s t)     -> register2 op "div" s t
      (Divu s t)    -> register2 op "divu" s t
      (Jalr s)      -> register op "jalr" s
      (Jr s)        -> register op "jr" s
      (Lis d)       -> register op "lis" d
      (Lw t o s)    -> memory op "lw" t o s
      (Mfhi d)      -> register op "mfhi" d
      (Mflo d)      -> register op "mflo" d
      (Mult s t)    -> register2 op "mult" s t
      (Multu s t)   -> register2 op "multu" s t
      (Slt d s t)   -> register3 op "slt" d s t
      (Sltu d s t)  -> register3 op "sltu" d s t
      (Sub d s t)   -> register3 op "sub" d s t
      (Sw t o s)    -> memory op "sw" t o s
      (Word w)      -> do
        instr <- pad ".word"
        word <- pad $ show w
        return $ ProgramLine op $ instr ++ word
    where pad x = do
            init <- listOf1 (arbitrary :: Gen Whitespace)
            end <- listOf1 (arbitrary :: Gen Whitespace)
            return $ (show init) ++ x ++ (show end)
          padR r = pad $ "$" ++ show r
          register3 op x d s t = do
            instr <- pad x
            reg1 <- padR d
            comma1 <- pad ","
            reg2 <- padR s
            comma2 <- pad ","
            reg3 <- padR t
            return $ ProgramLine op $ instr ++ reg1 ++ comma1 ++ reg2 ++ comma2 ++ reg3
          register2 op x s t = do
            instr <- pad x
            reg1 <- padR s
            comma <- pad ","
            reg2 <- padR t
            return $ ProgramLine op $ instr ++ reg1 ++ comma ++ reg2
          register op x s = do
            instr <- pad x
            reg <- padR s
            return $ ProgramLine op $ instr ++ reg
          offset op x s t o = do
            instr <- pad x
            reg1 <- padR s
            comma1 <- pad ","
            reg2 <- padR t
            comma2 <- pad ","
            off <- pad $ show o
            return $ ProgramLine op $ instr ++ reg1 ++ comma1 ++ reg2 ++ comma2 ++ off
          memory op x t o s = do
            instr <- pad x
            reg1 <- padR t
            comma1 <- pad ","
            off <- pad $ show o
            left <- pad "("
            reg2 <- padR s
            right <- pad ")"
            return $ ProgramLine op $ instr ++ reg1 ++ comma1 ++ off ++ left ++ reg2 ++ right
main = undefined
