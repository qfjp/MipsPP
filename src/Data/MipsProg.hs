-- TODO:
--  * change option (Nothing, Nothing) ... to something more robust.
--    As of now, we ignore any line that isnt something in the choice
--    list
module Data.MipsProg where

import           Padelude                hiding (option, try)

import           Data.Text               (pack)

import           Text.Parser.Char        (char, newline, noneOf)
import           Text.Parser.Combinators (choice, eof, option, optional, try)
import           Text.Parser.Token       (TokenParsing, symbolic)

import           Data.Instruction
import           Parser.Utils

type MipsProg = [Line]
type Line = (Maybe Instr, Maybe Text)

mipsProg :: (TokenParsing m, Monad m) => m MipsProg
mipsProg = do
    lines <- many line
    eof
    return lines

line :: (TokenParsing m, Monad m) => m Line
line = plusWhiteSpaceNoEnd $ do
    result <- option (Nothing, Nothing) $ choice [try instrThenComment, liftInstr, liftCommt]
    void newline
    return result

instrThenComment :: (TokenParsing m, Monad m) => m Line
instrThenComment = do
    instr <- instruction
    void $ optional whiteSpaceNoEnd
    commt <- comment
    return (Just instr, Just commt)

liftInstr :: (TokenParsing m, Monad m) => m Line
liftInstr = do
    instr <- instruction
    return (Just instr, Nothing)

liftCommt :: (TokenParsing m, Monad m) => m Line
liftCommt = do
    commt <- comment <|> (label >>= \x -> char ':' >> return x)
    return (Nothing, Just commt)

comment :: (TokenParsing m, Monad m) => m Text
comment = do
    void $ symbolic '#'
    str <- many $ noneOf endls
    return . pack $ str
