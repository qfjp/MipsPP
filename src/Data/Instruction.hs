module Data.Instruction (argImm) where

import           Padelude

import           Text.Parser.Char        (char, text)
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta.Parser    (Parser)

import           Data.Instruction.RType

import           Data.Register

import           Parser.Utils

data Instr =
    RInstr
  | I3 IInstr3 Reg Reg Integer
  | I2 IInstr2 Reg Integer
  | J3 JInstr3 Reg Reg Text
  | J2 JInstr2 Reg Text
  | J1 JInstr1 Text
  | S0 SInstr0
  | M3 MInstr3 Reg Reg Integer

data Argument = ArgR Reg | ArgI Integer | ArgL Text
  deriving (Show)

argReg :: Parser Argument
argReg = ArgR <$> register

argImm :: Parser Argument
argImm = ArgI <$> number
  where
      number = choice [integer, char '0' >> hexadecimal]

argLab :: Parser Argument
argLab = ArgL <$> label


data IInstr3 =
    Addi | Addiu | Andi | Ori | Sll | Slti | Sltiu | Sra | Srl | Xori
  deriving (Show, Eq, Ord, Enum)
data IInstr2 =
    Lui | Li
  deriving (Show, Eq, Ord, Enum)

data JInstr3 =
    Beq | Bne
  deriving (Show, Eq, Ord, Enum)
data JInstr2 =
    Bgez | Bgezal | Bgtz | Blez | Bltz | Bltzal
  deriving (Show, Eq, Ord, Enum)
data JInstr1 =
    J | Jal | Jr
  deriving (Show, Eq, Ord, Enum)

data SInstr0 =
    Noop | Syscall
  deriving (Show, Eq, Ord, Enum)

data MInstr3 =
    Lw | Sw | Lb | Sb
  deriving (Show, Eq, Ord, Enum)

