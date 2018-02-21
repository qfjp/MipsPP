{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Padelude                hiding (try)
import           Parser.Utils            (text')

import           Data.Text               (pack, toLower)
import           Text.Parser.Combinators
import           Text.Parser.Token       (someSpace, whiteSpace)
import           Text.Trifecta.Parser    (parseFromFileEx, parseString)
import           Text.Trifecta.Result    (Result)

import           Data.Instruction
import           Data.Instruction.JType
import           Data.MipsProg

main :: IO ()
main = do
    let str = parseString jtype mempty "jal endl"
    print (str :: Result JInstr)
    result <- parseFromFileEx mipsProg "test.s"
    print result
