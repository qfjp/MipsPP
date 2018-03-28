{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
-- TODO:
--  * change option (Nothing, Nothing) ... to something more robust.
--    As of now, we ignore any line that isnt something in the choice
--    list
module Data.MipsProg where

import           GHC.Exts                (IsList (..))
import           Padelude                hiding (option, toList, try)

import           Data.Text               (pack)

import           Text.Parser.Char        (char, newline, noneOf)
import           Text.Parser.Combinators (choice, eof, option, optional, try)
import           Text.Parser.Token       (TokenParsing)

import           Data.Instruction
import           Parser.Utils

import           Control.PPrint

data MipsProg = Empty | Lines Line MipsProg

instance IsList MipsProg where
    type Item MipsProg = Line
    fromList xs = foldr Lines Empty xs
    toList (Lines x y) = x:(toList y)
    toList Empty       = []

instance PPrint MipsProg where
    pprint (Lines l ls) = pprint l ++ "\n" ++ pprint ls
    pprint Empty        = ""

data Line = Instruction Instr
          | Comment Text
          | InstrComment Instr Text
          | Label Text
          | Directive Text
          | DataDecl Text
          | NoLine
        deriving (Show, Eq, Ord)

instance PPrint Line where
    pprint (Instruction x)    = pprint x
    pprint (Comment x)        = "#" ++ x
    pprint (InstrComment x y) = pprint x ++ "  # " ++ y
    pprint (Label x)          = x ++ ":"
    pprint (Directive x)      = "." ++ x
    pprint (DataDecl x)       = x
    pprint (NoLine)           = ""

mipsProg :: (TokenParsing m, Monad m) => m MipsProg
mipsProg = do
    lines <- many line
    eof
    return . fromList $ lines

line :: (TokenParsing m, Monad m) => m Line
line = plusWhiteSpaceNoEnd $ do
    result <- option (NoLine) $
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
    return (lab ++ ": ." ++ dir)

liftDataDecl :: (TokenParsing m, Monad m) => m Line
liftDataDecl = dataDeclaration >>= return . DataDecl

instrThenComment :: (TokenParsing m, Monad m) => m Line
instrThenComment = do
    instr <- instruction
    void $ optional whiteSpaceNoEnd
    commt <- comment
    return $ InstrComment instr commt

liftInstr :: (TokenParsing m, Monad m) => m Line
liftInstr = instruction >>= return . Instruction

liftCommt :: (TokenParsing m, Monad m) => m Line
liftCommt = comment >>= return . Comment

liftDir :: (TokenParsing m, Monad m) => m Line
liftDir = directive >>= return . Directive

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
    void $ char '#'
    str <- many $ noneOf endls
    return . pack $ str
