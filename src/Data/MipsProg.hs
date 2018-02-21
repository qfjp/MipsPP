module Data.MipsProg where

import           Padelude                hiding (option, try)

import           Data.Text               (pack)

import           Text.Parser.Char        (anyChar, newline)
import           Text.Parser.Combinators (eof, manyTill, option, optional, try)
import           Text.Parser.Token       (TokenParsing, symbolic)

import           Data.Instruction
import           Parser.Utils

type MipsProg = [Line]
type Line = (Maybe Instr, Maybe Comment)

mipsProg :: (TokenParsing m, Monad m) => m MipsProg
mipsProg = do
    lines <- many line
    eof
    return lines

line :: (TokenParsing m, Monad m) => m Line
line = do
    void $ optional whiteSpaceNoEnd
    instr <- option Nothing (Just <$> instruction)
    commt <- option Nothing (Just <$> comment)
    void newline
    return $ (instr, commt)

comment :: (TokenParsing m, Monad m) => m Text
comment = do
    void $ optional whiteSpaceNoEnd
    void $ symbolic '#'
    str <- manyTill anyChar (try whiteSpaceEnd)
    return . pack $ str
