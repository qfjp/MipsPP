module Data.Instruction where

import           Padelude

import           Text.Parser.Combinators (choice)
import           Text.Parser.Token       (TokenParsing)

import           Control.PPrint
import           Data.Instruction.IType
import           Data.Instruction.JType
import           Data.Instruction.MType
import           Data.Instruction.RType
import           Parser.Utils

data Instr =
    II IInstr
  | JI JInstr
  | MI MInstr
  | RI RInstr
  | Noop
  | Syscall
  deriving (Show, Eq, Ord)

instance PPrint Instr where
    pprint (II x)  = pprint x
    pprint (JI x)  = pprint x
    pprint (MI x)  = pprint x
    pprint (RI x)  = pprint x
    pprint Noop    = "noop"
    pprint Syscall = "syscall"

instruction :: (TokenParsing m, Monad m) => m Instr
instruction = do
    result <- choice
      [ II <$> itype
      , JI <$> jtype
      , MI <$> mtype
      , RI <$> rtype
      , text' "noop" >> return Noop
      , text' "syscall" >> return Syscall
      ]
    return result
