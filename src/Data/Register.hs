module Data.Register where

import           Padelude                hiding (try)
import           Parser.Utils            (plusWhiteSpace, text')
import           Prelude                 (show)
import           Text.Parser.Char        (char)
import           Text.Parser.Combinators (choice, try)
import           Text.Parser.Token       (TokenParsing)
import           Text.Trifecta.Parser    (Parser)

data Reg =
    Zero    | AT      | GP      | SP      | FP      | RA
  | Val Int | Arg Int | Tmp Int | Sav Int | Krn Int
  deriving (Eq, Ord)

instance Show Reg where
    show Zero    =  "$0"
    show AT      =  "$1"
    show GP      = "$28"
    show SP      = "$29"
    show FP      = "$30"
    show RA      = "$31"

    show (Val 0) =  "$2"
    show (Val 1) =  "$3"
    show (Val _) = undefined

    show (Arg 0) =  "$4"
    show (Arg 1) =  "$5"
    show (Arg 2) =  "$6"
    show (Arg 3) =  "$7"
    show (Arg _) = undefined

    show (Tmp 0) =  "$8"
    show (Tmp 1) =  "$9"
    show (Tmp 2) = "$10"
    show (Tmp 3) = "$11"
    show (Tmp 4) = "$12"
    show (Tmp 5) = "$13"
    show (Tmp 6) = "$14"
    show (Tmp 7) = "$15"
    show (Tmp 8) = "$23"
    show (Tmp 9) = "$24"
    show (Tmp _) = undefined

    show (Sav 0) = "$16"
    show (Sav 1) = "$17"
    show (Sav 2) = "$18"
    show (Sav 3) = "$19"
    show (Sav 4) = "$20"
    show (Sav 5) = "$21"
    show (Sav 6) = "$22"
    show (Sav 7) = "$23"
    show (Sav _) = undefined

    show (Krn 0) = "$26"
    show (Krn 1) = "$27"
    show (Krn _) = undefined


regParse :: (TokenParsing m, Monad m) => Reg -> m Reg
regParse r@Zero    = regText r ["zero",  "0"]
regParse r@AT      = regText r ["at",    "1"]
regParse r@GP      = regText r ["gp",   "28"]
regParse r@SP      = regText r ["sp",   "29"]
regParse r@FP      = regText r ["fp",   "30"]
regParse r@RA      = regText r ["ra",   "31"]

regParse r@(Val 0) = regText r ["v0",    "2"]
regParse r@(Val 1) = regText r ["v1",    "3"]
regParse   (Val _) = undefined

regParse r@(Arg 0) = regText r ["a0",    "4"]
regParse r@(Arg 1) = regText r ["a1",    "5"]
regParse r@(Arg 2) = regText r ["a2",    "6"]
regParse r@(Arg 3) = regText r ["a3",    "7"]
regParse   (Arg _) = undefined

regParse r@(Tmp 0) = regText r ["t0",    "8"]
regParse r@(Tmp 1) = regText r ["t1",    "9"]
regParse r@(Tmp 2) = regText r ["t2",   "10"]
regParse r@(Tmp 3) = regText r ["t3",   "11"]
regParse r@(Tmp 4) = regText r ["t4",   "12"]
regParse r@(Tmp 5) = regText r ["t5",   "13"]
regParse r@(Tmp 6) = regText r ["t6",   "14"]
regParse r@(Tmp 7) = regText r ["t7",   "15"]
regParse r@(Tmp 8) = regText r ["t8",   "23"]
regParse r@(Tmp 9) = regText r ["t9",   "24"]
regParse   (Tmp _) = undefined

regParse r@(Sav 0) = regText r ["s0",   "16"]
regParse r@(Sav 1) = regText r ["s1",   "17"]
regParse r@(Sav 2) = regText r ["s2",   "18"]
regParse r@(Sav 3) = regText r ["s3",   "19"]
regParse r@(Sav 4) = regText r ["s4",   "20"]
regParse r@(Sav 5) = regText r ["s5",   "21"]
regParse r@(Sav 6) = regText r ["s6",   "22"]
regParse r@(Sav 7) = regText r ["s7",   "23"]
regParse   (Sav _) = undefined

regParse r@(Krn 0) = regText r ["k0",   "26"]
regParse r@(Krn 1) = regText r ["k1",   "27"]
regParse   (Krn _) = undefined

register :: (TokenParsing m, Monad m) => m Reg
register = choice . map (try $) $
  [ regParse Zero
  , regParse AT
  , regParse GP
  , regParse SP
  , regParse FP
  , regParse RA

  , regParse (Val 0)
  , regParse (Val 1)

  , regParse (Arg 0)
  , regParse (Arg 1)
  , regParse (Arg 2)
  , regParse (Arg 3)

  , regParse (Tmp 0)
  , regParse (Tmp 1)
  , regParse (Tmp 2)
  , regParse (Tmp 3)
  , regParse (Tmp 4)
  , regParse (Tmp 5)
  , regParse (Tmp 6)
  , regParse (Tmp 7)
  , regParse (Tmp 8)
  , regParse (Tmp 9)

  , regParse (Sav 0)
  , regParse (Sav 1)
  , regParse (Sav 2)
  , regParse (Sav 3)
  , regParse (Sav 4)
  , regParse (Sav 5)
  , regParse (Sav 6)
  , regParse (Sav 7)

  , regParse (Krn 0)
  , regParse (Krn 1)
  ]


regText :: (TokenParsing m, Monad m) => Reg -> [Text] -> m Reg
regText r xs = plusWhiteSpace (char '$' >> (choice . map text') xs) >> return r

--register :: Parser Text
--register = do
--    symbolic '$'
--    reg <- manyTill anyChar (try space)
--    whiteSpace
--    return . pack $ ('$':reg)
