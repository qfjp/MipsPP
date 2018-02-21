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

data Line = InstrComment (Maybe Instr, Maybe Text)
          | Label Text
          | Directive Text
          | DataDecl Text
        deriving (Show, Eq, Ord)

mipsProg :: (TokenParsing m, Monad m) => m MipsProg
mipsProg = do
    lines <- many line
    eof
    return lines

line :: (TokenParsing m, Monad m) => m Line
line = plusWhiteSpaceNoEnd $ do
    result <- option (InstrComment (Nothing, Nothing)) $
        choice [ try instrThenComment
               , liftInstr
               , liftCommt
               , liftDir
               , try liftDataDecl
               , liftLabel
               ]
    void newline
    return result

dataDeclaration :: (TokenParsing m, Monad m) => m Text
dataDeclaration = do
    lab <- label
    void $ char ':'
    void $ whiteSpaceNoEnd
    dir <- directive
    return (lab ++ ": " ++ dir)

liftDataDecl :: (TokenParsing m, Monad m) => m Line
liftDataDecl = dataDeclaration >>= return . DataDecl

instrThenComment :: (TokenParsing m, Monad m) => m Line
instrThenComment = do
    instr <- instruction
    void $ optional whiteSpaceNoEnd
    commt <- comment
    return $ InstrComment (Just instr, Just commt)

liftInstr :: (TokenParsing m, Monad m) => m Line
liftInstr = do
    instruction >>= return . InstrComment . (flip (,)) Nothing . Just

liftCommt :: (TokenParsing m, Monad m) => m Line
liftCommt = do
    commt <- comment
    return $ InstrComment (Nothing, Just commt)

liftDir :: (TokenParsing m, Monad m) => m Line
liftDir = do
    dir <- directive
    return $ Directive dir

liftLabel :: (TokenParsing m, Monad m) => m Line
liftLabel = do
    lab <- label
    void $ char ':'
    return $ Label lab

directive :: (TokenParsing m, Monad m) => m Text
directive = plusWhiteSpaceNoEnd $ do
    void $ char '.'
    str <- many $ noneOf endls
    return . pack $ str

comment :: (TokenParsing m, Monad m) => m Text
comment = do
    void $ symbolic '#'
    str <- many $ noneOf endls
    return . pack $ str
