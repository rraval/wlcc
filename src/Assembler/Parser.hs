module Assembler.Parser where

import Assembler.Data(Generation(..), Operation(..), MipsWord, Offset, Register)
import Control.Monad(when)
import Data.Bits(complement)
import Data.Char(digitToInt)
import Data.List(foldl')
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

type AsmParser a = CharParser Generation a
parser :: AsmParser [Operation]
parser = do
    whiteSpace
    ops <- many line
    (eof <?> "End of file")
    return ops

line :: AsmParser Operation
line = do
    many label
    op <- instruction
    eol
    return op
  where eol = newline <|> (char '\r' >> char '\n')

label :: AsmParser String
label =
    lexeme (do
        id <- many1 letter
        gen <- getState
        case M.lookup id $ labelTable gen of
            Just x -> fail $ "Duplicate Label: '" ++ id ++ "'"
            Nothing -> setState $ gen { labelTable = M.insert id (wordOffset gen) (labelTable gen) }
        char ':'
        return id)
    <?> "Label"

instruction :: AsmParser Operation
instruction =
    do
        op <- operation
        case op of
            "add"   -> register3 >>= \(d, s, t) -> return $ Add d s t
            "beq"   -> offsetOrLabel (Beq, BeqL)
            "bne"   -> offsetOrLabel (Bne, BneL)
            "div"   -> register2 >>= \(s, t) -> return $ Div s t
            "divu"  -> register2 >>= \(s, t) -> return $ Divu s t
            "jalr"  -> register >>= \s -> return $ Jalr s
            "jr"    -> register >>= \s -> return $ Jr s
            "lis"   -> register >>= \d -> return $ Lis d
            "lw"    -> loadOrStore Lw
            "mfhi"  -> register >>= \d -> return $ Mfhi d
            "mflo"  -> register >>= \d -> return $ Mflo d
            "mult"  -> register2 >>= \(s, t) -> return $ Mult s t
            "multu" -> register2 >>= \(s, t) -> return $ Multu s t
            "slt"   -> register3 >>= \(d, s, t) -> return $ Slt d s t
            "sltu"  -> register3 >>= \(d, s, t) -> return $ Sltu d s t
            "sub"   -> register3 >>= \(d, s, t) -> return $ Sub d s t
            "sw"    -> loadOrStore Sw
            ".word" -> number >>= \n -> return $ Word n
    <?> "Instruction"
  where register =
            do
                char '$'
                -- registers may only be specified in base 10
                reg <- many1 digit
                let reg' = toBase 10 reg
                if reg' > 31
                    then fail $ "No such register: $" ++ reg
                    else return (fromIntegral reg' :: Register)
            <?> "Register"
        register2 = do
            s <- register
            comma
            t <- register
            return (s, t)
        register3 = do
            (d, s) <- register2
            comma
            t <- register
            return (d, s, t)
        offsetOrLabel (offsetConstructor, labelConstructor) = do
            (s, t) <- register2
            comma
            do
                    label <- try identifier
                    -- label constructor is second
                    return $ labelConstructor s t label
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
        operation = many1 (letter <|> char '.')

number :: AsmParser MipsWord
number =
    lexeme (do
            char '-'
            n <- many1 digit >>= check . toBase 10
            check $ (fromIntegral $ complement n + 1:: Integer)
        <|> do
            try $ string "0x"
            many1 hexDigit >>= check . toBase 16
        <|> (many1 digit >>= check . toBase 10)
        <?> "Number")
  where check n = if n > (fromIntegral (maxBound :: MipsWord) :: Integer)
                    then fail "Number too large"
                    else return (fromIntegral n :: MipsWord)

toBase :: Integer -> String -> Integer
toBase base = foldl' (\acc -> \x -> acc * base + (fromIntegral $ digitToInt x :: Integer)) 0

comma = P.comma lexer
identifier = P.identifier lexer
lexeme = P.lexeme lexer
natural = P.natural lexer
parens = P.parens lexer
whiteSpace = P.whiteSpace lexer

lexer :: P.TokenParser Generation
lexer = P.makeTokenParser(asmDef)

asmDef = emptyDef {
    commentLine = ";",
    identStart = letter,
    identLetter = letter,
    caseSensitive = False
}