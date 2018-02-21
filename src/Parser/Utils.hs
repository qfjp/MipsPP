module Parser.Utils where

import           Data.Char               (Char, isSpace, toLower, toUpper)
import           Padelude                hiding (show, try)
import           Prelude                 (String, show)

import           Data.Text               (cons, pack, unpack)

import           Text.Parser.Char        (CharParsing, char, newline, satisfy)
import           Text.Parser.Combinators (choice, eof, skipSome, try, (<?>))
import           Text.Parser.Token       (TokenParsing, hexadecimal, integer')

data Imm = ILabel Text | IConst Integer
  deriving (Show, Eq, Ord)

plusWhiteSpaceNoEnd :: (Monad m, TokenParsing m)  => m a -> m a
plusWhiteSpaceNoEnd p = do
    p >>= \result -> optional whiteSpaceNoEnd >> return result

-- | A case insensitive parser that needs at least one char
string' :: (Monad m, CharParsing m) => String -> m String
string' s = do
    s <$ try (traverse_ charCaseless s) <?> show s

text' :: (Monad m, CharParsing m) => Text -> m Text
text' t = t <$ string' (unpack t)

charCaseless :: CharParsing m => Char -> m Char
charCaseless = choice . (mapM ($) (map (char .) [toUpper, toLower]))

label :: (Monad m, TokenParsing m) => m Text
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

number :: (TokenParsing m, Monad m) => m Integer
number = choice $
    [integer', char '0' >> hexadecimal]

immediate :: (TokenParsing m, Monad m) => m Imm
immediate = choice $
    [ number >>= return . IConst
    , label  >>= return . ILabel
    ]

endls :: [Char]
endls = ['\v', '\r', '\f', '\n']

isSpaceNoEndl :: Char -> Bool
isSpaceNoEndl c = isSpace c && (not (c `elem` endls))

-- foldr :: (f a -> f (t -> t) -> f (t-> t)) -> b -> t a -> b

whiteSpaceNoEnd :: (TokenParsing m, Monad m) => m Text
whiteSpaceNoEnd = skipSome (satisfy isSpaceNoEndl) >> return ""

whiteSpaceEnd :: (TokenParsing m, Monad m) => m Text
whiteSpaceEnd = do
    choice [void newline, eof]
    return ""

parseDataAsStrings :: (TokenParsing m, Monad m) => a -> [Text] -> m a
parseDataAsStrings r xs = ((choice . map text') xs) >> return r

parseDataAsShow :: (TokenParsing m, Monad m, Show a) => a -> m a
parseDataAsShow i = plusWhiteSpaceNoEnd $ parseDataAsStrings i [pack . show $ i]

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)
