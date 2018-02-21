module Data.Instruction.IType where

import           Padelude                hiding (try)

import           Data.Text               (toLower)
import qualified Text.Show               as T (show)

import           Text.Parser.Combinators (choice, count)
import           Text.Parser.Token       (TokenParsing)

import           Control.PPrint
import           Data.Register
import           Parser.Utils

data IInstr =
    II3 IText3 Imm Reg Reg
  | II2 IText2 Imm Reg
  deriving (Show, Eq, Ord)

instance PPrint IInstr where
    pprint (II3 instr im r1 r2) =
        pprint instr ++ " " ++
        pprint r1 ++ ", " ++
        pprint r2 ++ ", " ++
        pprint im
    pprint (II2 instr im r1) =
        pprint instr ++ " " ++
        pprint r1 ++ ", " ++
        pprint im

data IText3 =
    Addi | Addiu | Andi | Ori | Sll | Slti | Sltiu | Sra | Srl | Xori
  deriving (Show, Eq, Ord, Enum)

data IText2 =
    Lui | La | Li
  deriving (Show, Eq, Ord, Enum)

data IText =
    IT3 IText3 | IT2 IText2
  deriving (Eq, Ord)

instance PPrint IText3 where
    pprint = toLower . show
instance PPrint IText2 where
    pprint = toLower . show

instance Show IText where
    show (IT3 r) = T.show r
    show (IT2 r) = T.show r

itype :: (TokenParsing m, Monad m) => m IInstr
itype = choice . map makeItype $
  [ IT3 Addiu
  , IT3 Addi
  , IT3 Andi
  , IT3 Ori
  , IT3 Sll
  , IT3 Sltiu
  , IT3 Slti
  , IT3 Sra
  , IT3 Srl
  , IT3 Xori

  , IT2 Lui
  , IT2 La
  , IT2 Li
  ]

makeItype :: (TokenParsing m, Monad m) => IText -> m IInstr
makeItype i@(IT3 _) = makeItypeN (parseArgsN 2) i
makeItype i@(IT2 _) = makeItypeN (parseArgsN 1) i

makeItypeN :: (TokenParsing m, Monad m) => m (Imm, [Reg]) -> IText -> m IInstr
makeItypeN parse i = do
    void $ parseDataAsShow i
    (imm, regs) <- parse
    let maybInstr = regsToIInstr i imm regs
    case maybInstr of
      Just x  -> return x
      Nothing -> undefined

regsToIInstr :: IText -> Imm -> [Reg] -> Maybe IInstr
regsToIInstr (IT3 instr) imm [r1, r2] = Just $
    II3 instr imm r1 r2
regsToIInstr (IT2 instr) imm [r] = Just $
    II2 instr imm r
regsToIInstr _ _ _ = Nothing

parseArgsN :: (TokenParsing m, Monad m) => Int -> m (Imm, [Reg])
parseArgsN n = do
    regs <- count n register
    imm <- immediate
    return (imm, regs)
