module Data.Register where

import           Padelude                hiding (K1, show, try)
import           Prelude                 (show)

import           Data.Text               (pack, toLower)

import           Control.PPrint
import           Parser.Utils            (parseDataAsStrings,
                                          plusWhiteSpaceNoEnd, text', (<<))
import           Text.Parser.Char        (char)
import           Text.Parser.Combinators (choice, try)
import           Text.Parser.Token       (TokenParsing)

data Reg =
    Zero | AT | GP | SP | FP | RA
  | V0   | V1
  | A0   | A1 | A2 | A3
  | T0   | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
  | S0   | S1 | S2 | S3 | S4 | S5 | S6 | S7
  | K0   | K1
  deriving (Eq, Ord)

instance Show Reg where
    show Zero =  "$0"
    show AT   =  "$1"
    show GP   = "$28"
    show SP   = "$29"
    show FP   = "$30"
    show RA   = "$31"

    show V0   =  "$2"
    show V1   =  "$3"

    show A0   =  "$4"
    show A1   =  "$5"
    show A2   =  "$6"
    show A3   =  "$7"

    show T0   =  "$8"
    show T1   =  "$9"
    show T2   = "$10"
    show T3   = "$11"
    show T4   = "$12"
    show T5   = "$13"
    show T6   = "$14"
    show T7   = "$15"
    show T8   = "$23"
    show T9   = "$24"

    show S0   = "$16"
    show S1   = "$17"
    show S2   = "$18"
    show S3   = "$19"
    show S4   = "$20"
    show S5   = "$21"
    show S6   = "$22"
    show S7   = "$23"

    show K0   = "$26"
    show K1   = "$27"

instance PPrint Reg where
    pprint = toLower . pack . show


regParse :: (TokenParsing m, Monad m) => Reg -> m Reg
regParse r@Zero = parseDataAsStrings r ["$zero",  "$0"]
regParse r@AT   = parseDataAsStrings r ["$at",    "$1"]
regParse r@GP   = parseDataAsStrings r ["$gp",   "$28"]
regParse r@SP   = parseDataAsStrings r ["$sp",   "$29"]
regParse r@FP   = parseDataAsStrings r ["$fp",   "$30"]
regParse r@RA   = parseDataAsStrings r ["$ra",   "$31"]

regParse r@V0   = parseDataAsStrings r ["$v0",    "$2"]
regParse r@V1   = parseDataAsStrings r ["$v1",    "$3"]

regParse r@A0   = parseDataAsStrings r ["$a0",    "$4"]
regParse r@A1   = parseDataAsStrings r ["$a1",    "$5"]
regParse r@A2   = parseDataAsStrings r ["$a2",    "$6"]
regParse r@A3   = parseDataAsStrings r ["$a3",    "$7"]

regParse r@T0   = parseDataAsStrings r ["$t0",    "$8"]
regParse r@T1   = parseDataAsStrings r ["$t1",    "$9"]
regParse r@T2   = parseDataAsStrings r ["$t2",   "$10"]
regParse r@T3   = parseDataAsStrings r ["$t3",   "$11"]
regParse r@T4   = parseDataAsStrings r ["$t4",   "$12"]
regParse r@T5   = parseDataAsStrings r ["$t5",   "$13"]
regParse r@T6   = parseDataAsStrings r ["$t6",   "$14"]
regParse r@T7   = parseDataAsStrings r ["$t7",   "$15"]
regParse r@T8   = parseDataAsStrings r ["$t8",   "$23"]
regParse r@T9   = parseDataAsStrings r ["$t9",   "$24"]

regParse r@S0   = parseDataAsStrings r ["$s0",   "$16"]
regParse r@S1   = parseDataAsStrings r ["$s1",   "$17"]
regParse r@S2   = parseDataAsStrings r ["$s2",   "$18"]
regParse r@S3   = parseDataAsStrings r ["$s3",   "$19"]
regParse r@S4   = parseDataAsStrings r ["$s4",   "$20"]
regParse r@S5   = parseDataAsStrings r ["$s5",   "$21"]
regParse r@S6   = parseDataAsStrings r ["$s6",   "$22"]
regParse r@S7   = parseDataAsStrings r ["$s7",   "$23"]

regParse r@K0   = parseDataAsStrings r ["$k0",   "$26"]
regParse r@K1   = parseDataAsStrings r ["$k1",   "$27"]

register :: (TokenParsing m, Monad m) => m Reg
register = choice . map ((try $) . plusWhiteSpaceNoEnd . regParse) $
  [ Zero
  , AT,  GP, SP, FP, RA
  , V0,  V1
  , A0,  A1, A2, A3
  , T0,  T1, T2, T3, T4, T5, T6, T7, T8, T9
  , S0,  S1, S2, S3, S4, S5, S6, S7
  , K0,  K1
  ]

regText :: (TokenParsing m, Monad m) => Reg -> [Text] -> m Reg
regText r xs = (return r) << (plusWhiteSpaceNoEnd $ do
    void $ char '$'
    choice . map text' $ xs)
