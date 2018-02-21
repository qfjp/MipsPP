{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Padelude                hiding (try)
import           Parser.Utils            (text')

import           Data.Text               (pack, toLower)
import           Text.Parser.Combinators
import           Text.Parser.Token       (someSpace, whiteSpace)
import           Text.Trifecta.Parser    (parseFromFileEx, parseString)
import           Text.Trifecta.Result    (Result (..))

import           Control.PPrint
import           Data.Instruction
import           Data.Instruction.JType
import           Data.MipsProg

main :: IO ()
main = do
    let str = parseString jtype mempty "jal endl"
    print (str :: Result JInstr)
    result <- parseFromFileEx mipsProg "test.s"
    case result of
      Success x -> putStrLn (pprint x)
      Failure x -> print x
