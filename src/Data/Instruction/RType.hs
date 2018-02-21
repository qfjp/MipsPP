module Data.Instruction.RType where

import           Padelude                hiding (try)

import           Data.Text               (toLower)
import qualified Text.Show               as T (show)

import           Text.Parser.Combinators (choice, count)
import           Text.Parser.Token       (TokenParsing)

import           Control.PPrint
import           Data.Register
import           Parser.Utils

data RInstr =
    RI3 RText3 Reg Reg Reg
  | RI2 RText2 Reg Reg
  | RI1 RText1 Reg
  deriving (Show, Eq, Ord)

instance PPrint RInstr where
    pprint (RI3 instr r1 r2 r3) =
        pprint instr ++ " " ++
        pprint r1 ++ ", " ++
        pprint r2 ++ ", " ++
        pprint r3
    pprint (RI2 instr r1 r2) =
        pprint instr ++ " " ++
        pprint r1 ++ ", " ++
        pprint r2
    pprint (RI1 instr r1) =
        pprint instr ++ " " ++
        pprint r1

data RText =
    RT3 RText3 | RT2 RText2 | RT1 RText1
  deriving (Eq, Ord)

instance Show RText where
    show (RT3 r) = T.show r
    show (RT2 r) = T.show r
    show (RT1 r) = T.show r

data RText3 =
    Add | Addu | And | Or | Sllv | Slt | Sltu | Srlv | Sub | Subu | Xor
  deriving (Show, Eq, Ord, Enum)

data RText2 =
    Div | Divu | Mult | Multu
  deriving (Show, Eq, Ord, Enum)

data RText1 =
    Mfhi | Mflo
  deriving (Show, Eq, Ord, Enum)

instance PPrint RText3 where
    pprint = toLower . show
instance PPrint RText2 where
    pprint = toLower . show
instance PPrint RText1 where
    pprint = toLower . show

rtype :: (TokenParsing m, Monad m) => m RInstr
rtype = choice . map makeRtype $
  [ RT3 Addu
  , RT3 Add
  , RT3 And
  , RT3 Or
  , RT3 Sllv
  , RT3 Sltu
  , RT3 Slt
  , RT3 Srlv
  , RT3 Subu
  , RT3 Sub
  , RT3 Xor

  , RT2 Divu
  , RT2 Div
  , RT2 Multu
  , RT2 Mult

  , RT1 Mfhi
  , RT1 Mflo
  ]

makeRtype :: (TokenParsing m, Monad m) => RText -> m RInstr
makeRtype i@(RT3 _) = makeRtypeN (parseArgsN 3) i
makeRtype i@(RT2 _) = makeRtypeN (parseArgsN 2) i
makeRtype i@(RT1 _) = makeRtypeN (parseArgsN 1) i

makeRtypeN :: (TokenParsing m, Monad m) => m ((), [Reg]) -> RText -> m RInstr
makeRtypeN parse i = do
    void $ parseDataAsShow i
    (_, rs) <- parse
    let maybInstr = regsToRInstr i $ rs
    case maybInstr of
      Just x  -> return x
      Nothing -> undefined

regsToRInstr :: RText -> [Reg] -> Maybe RInstr
regsToRInstr (RT3 i) [r1, r2, r3] = Just $
    RI3 i r1 r2 r3
regsToRInstr (RT2 i) [r1, r2] = Just $
    RI2 i r1 r2
regsToRInstr (RT1 i) [r] = Just $
    RI1 i r
regsToRInstr _ _ = Nothing

parseArgsN :: (TokenParsing m, Monad m) => Int -> m ((), [Reg])
parseArgsN n =
    sequence ((), count n register)
