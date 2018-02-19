{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Padelude                hiding (try)
import           Parser.Utils            (text')

import           Data.Text               (pack, toLower)
import           Text.Parser.Char        hiding (space)
import qualified Text.Parser.Char        as C (space)
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta.Parser    (Parser, parseFromFile, parseString)

import           Data.Instruction
import           Data.Instruction.RType
import           Data.Register


end :: Parser ()
end =
    void newline <|> eof

space :: Parser ()
space
  = void C.space <|> eof

comment :: Parser Text
comment = do
    symbolic '#'
    str <- manyTill anyChar (try end)
    return . pack $ str


--parseInstrN :: Int -> Parser Argument
--parseInstrN n = do
--    --instr <- instrArity3
--    instr <- undefined
--    whiteSpace
--    arg1 <- standardArg
--    let append  arg prs = prs >>= \p -> return $ arg ++ ", " ++ p
--    foldlM append arg1 (replicate (n - 1) standardArg)

listToParser :: [Text] -> Parser Text
listToParser (x:xs) =
    foldr (\x y -> text x <|> y) (text x) xs

main :: IO ()
main = do
  result <- return $ parseString rtype mempty "addu $s3 $s4 $t0"
  print result
