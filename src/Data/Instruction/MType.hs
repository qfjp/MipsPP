module Data.Instruction.MType where

import           Padelude                hiding (show, try)

import           Text.Parser.Char        (char)
import           Text.Parser.Combinators (choice)
import           Text.Parser.Token       (TokenParsing)

import           Data.Register
import           Parser.Utils

data MInstr = MI3 MText Imm Reg Reg
  deriving (Show, Eq, Ord)

data MText =
    Lw | Sw | Lb | Sb
  deriving (Show, Eq, Ord, Enum)

mtype :: (TokenParsing m, Monad m) => m MInstr
mtype = choice . map (makeMtype parseArgs) $
  [ Lw
  , Sw
  , Lb
  , Sb
  ]

makeMtype :: (TokenParsing m, Monad m) => m (Imm, [Reg]) -> MText -> m MInstr
makeMtype parse i = do
    void $ parseDataAsShow i
    (imm, regs) <- parse
    let maybInstr = regsToMInstr i imm regs
    case maybInstr of
      Just x  -> return x
      Nothing -> undefined

regsToMInstr :: MText -> Imm -> [Reg] -> Maybe MInstr
regsToMInstr instr imm [r1, r2] = Just $
    MI3 instr imm r1 r2
regsToMInstr _ _ _ = Nothing

parseArgs :: (TokenParsing m, Monad m) => m (Imm, [Reg])
parseArgs = do
    reg1 <- register
    imm  <- immediate
    void $ char '('
    reg2 <- register
    void $ char ')'
    return (imm, [reg1, reg2])
