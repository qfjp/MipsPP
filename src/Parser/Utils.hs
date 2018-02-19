module Parser.Utils where

import           Data.Char               (Char, toLower, toUpper)
import           Padelude                hiding (show, try)
import           Prelude                 (String, show)

import           Data.Text               (cons, pack, unpack)

import           Text.Parser.Char        (CharParsing, char, satisfy)
import           Text.Parser.Combinators (choice, try, (<?>))
import           Text.Parser.Token       (TokenParsing, whiteSpace)
import           Text.Trifecta.Parser    (Parser)

plusWhiteSpace :: (Monad m, TokenParsing m)  => m Text -> m Text
plusWhiteSpace p = do
    p >>= \result -> whiteSpace >> return result

-- | A case insensitive parser
string' :: CharParsing m => String -> m String
string' s = s <$ try (traverse_ charCaseless s) <?> show s

text' :: CharParsing m => Text -> m Text
text' t = t <$ string' (unpack t)

charCaseless :: CharParsing m => Char -> m Char
charCaseless = choice . (mapM ($) (map (char .) [toUpper, toLower]))

label :: (Monad m, CharParsing m) => m Text
label = do
    lead <- satisfy isLeadChar
    rest <- many . satisfy $ isIdentChar
    return (lead `cons` pack rest)
  where
      isLeadChar c
        | 65 <= ord c && ord c <=  90 = True -- [A-Z]
        | 97 <= ord c && ord c <= 122 = True -- [a-z]
        | otherwise = False
      isIdentChar c
        | 48 <= ord c && ord c <= 57 = True  -- [0-9]
        | ord c == 95 = True                 -- [_]
        | otherwise = isLeadChar c
