module Assembler.Parser(parser) where

import Assembler.Data(Generation(..), Operation(..), MipsWord, Offset, Register)
import Control.Monad(when)
import Data.Bits(complement)
import Data.Char(digitToInt, isSpace)
import Data.List(foldl')
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (label)

type AsmParser a = CharParser Generation a

parser :: AsmParser (Generation, [Operation])
parser = do
  whiteSpace
  many label
  ops <- many line
  gen <- getState
  (eof <?> "End of file")
  return (gen, ops)

line :: AsmParser Operation
line = do
  op <- instruction
  many label
  return op

label :: AsmParser String
label = lexeme label' <?> "Label"
  where label' = do
          id <- many1 letter
          gen <- getState
          case M.lookup id $ labelTable gen of
            Just x -> error $ "Duplicate Label: '" ++ id ++ "'"
            Nothing -> setState $ gen { labelTable = M.insert id (wordOffset gen) (labelTable gen) }
          char ':'
          return id

instruction :: AsmParser Operation
instruction = do op <- instruction'
                 whiteSpace
                 return op
              <?> "Instruction"
  where instruction' = do
          op <- operation
          updateState $ \gen -> gen { wordOffset = 1 + wordOffset gen }
          case op of
            "add"   -> register3 Add
            "beq"   -> offsetOrLabel (Beq, BeqL)
            "bne"   -> offsetOrLabel (Bne, BneL)
            "div"   -> register2 Div
            "divu"  -> register2 Divu
            "jalr"  -> register >>= return . Jalr
            "jr"    -> register >>= return . Jr
            "lis"   -> register >>= return . Lis
            "lw"    -> loadOrStore Lw
            "mfhi"  -> register >>= return . Mfhi
            "mflo"  -> register >>= return . Mflo
            "mult"  -> register2 Mult
            "multu" -> register2 Multu
            "slt"   -> register3 Slt
            "sltu"  -> register3 Sltu
            "sub"   -> register3 Sub
            "sw"    -> loadOrStore Sw
            ".word" -> number >>= return . Word
            _       -> fail $ "Unknown instruction: " ++ op
        register = do
          char '$' <?> "Register"
          -- registers may only be specified in base 10
          reg <- lexeme (many1 digit) <?> "Register number"
          let reg' = toBase 10 reg
          if reg' > 31
            then fail $ "No such register: $" ++ reg
            else return (fromIntegral reg' :: Register)
        register2 cons = do
          s <- register
          comma
          t <- register
          return $ cons s t
        register3 cons = do
          d <- register
          comma
          s <- register
          comma
          register >>= return . cons d s
        offsetOrLabel (offsetConstructor, labelConstructor) = do
          s <- register
          comma
          t <- register
          comma
          do
            label <- try identifier
            gen <- getState
            let currentpos = wordOffset gen
            return $ labelConstructor s t label currentpos
            <|> do
              i <- number
              checkOffset i
              return $ offsetConstructor s t (fromIntegral i :: Offset)
            <?> "Offset or Label"
        loadOrStore cons = do
            t <- register
            comma
            i <- (number <?> "Offset")
            checkOffset i
            s <- parens register
            return $ cons t (fromIntegral i :: Offset) s
        checkOffset i = if i > (fromIntegral (maxBound :: Offset) :: MipsWord)
                          then fail "Offset too large to fit"
                          else return ()
        operation = (lexeme $ many1 (letter <|> char '.')) <?> "Operation"

number :: AsmParser MipsWord
number = lexeme  number' <?> "Number"
  where number' = do
              char '-'
              n <- many1 digit >>= check . toBase 10
              check $ (fromIntegral $ complement n + 1 :: Integer)
          <|> do
            try $ string "0x"
            many1 hexDigit >>= check . toBase 16
          <|> (many1 digit >>= check . toBase 10)
        check n = if n > (fromIntegral (maxBound :: MipsWord) :: Integer)
                    then fail "Number too large"
                    else return (fromIntegral n :: MipsWord)

toBase :: Integer -> String -> Integer
toBase base = foldl' (\acc x -> acc * base + (fromIntegral $ digitToInt x :: Integer)) 0

comma = lexeme (char ',') <?> "Comma"
identifier = many1 letter
parens p = do
  lexeme (char '(') <?> "'('"
  ret <- p
  lexeme (char ')') <?> "')'"
  return ret

lexeme p = do r <- try p
              whiteSpace
              return r

comment :: AsmParser String
comment = do
    char ';'
    many $ satisfy (/= '\n')
whiteSpace = many (many1 space <|> comment) <?> "Whitespace"

tryParse p s = runParser p (Generation M.empty 0) "" s
