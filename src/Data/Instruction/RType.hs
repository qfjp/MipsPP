module Data.Instruction.RType where

import           Padelude                hiding (show, try)

import           Data.Text               (pack, toLower)
import           Text.Show               (show)

import           Text.Parser.Char        (CharParsing)
import           Text.Parser.Combinators (choice, try)
import           Text.Parser.Token       (TokenParsing)
import           Text.Trifecta.Parser    (Parser)

import           Data.Register
import           Parser.Utils

data RInstr =
    RI3 RText3 Reg Reg Reg
  | RI2 RText2 Reg Reg
  | RI1 RText1 Reg
  deriving (Show, Eq, Ord)

data RText =
    RT3 RText3 | RT2 RText2 | RT1 RText1
  deriving (Eq, Ord)

instance Show RText where
    show (RT3 r) = show r
    show (RT2 r) = show r
    show (RT1 r) = show r

data RText3 =
    Add | Addu | And | Or | Sllv | Slt | Sltu | Srlv | Sub | Subu | Xor
  deriving (Show, Eq, Ord, Enum)

data RText2 =
    Div | Divu | Mult | Multu
  deriving (Show, Eq, Ord, Enum)

data RText1 =
    Mfhi | Mflo
  deriving (Show, Eq, Ord, Enum)

rtypeText :: (TokenParsing m, Monad m) => RText -> [Text] -> m RText
rtypeText r xs = plusWhiteSpace ((choice . map text') xs) >> return r

rtypeParse :: (TokenParsing m, Monad m) => RText -> m RText
rtypeParse i = rtypeText i [pack . show $ i]

rtype :: (TokenParsing m, Monad m) => m RInstr
rtype = choice . map (try $) $
    -- foldl :: (b -> Reg -> b) -> b -> [Reg] -> b
    -- foldlM :: (b -> Reg -> (Reg -> b)) -> b -> [Reg] -> (Reg -> b)
  [ rtypeParse (RT3 Add)   >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Add  r1 r2 r3)))
  , rtypeParse (RT3 Addu)  >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Addu r1 r2 r3)))
  , rtypeParse (RT3 And)   >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  And  r1 r2 r3)))
  , rtypeParse (RT3 Or)    >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Or   r1 r2 r3)))
  , rtypeParse (RT3 Sllv)  >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Sllv r1 r2 r3)))
  , rtypeParse (RT3 Slt)   >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Slt  r1 r2 r3)))
  , rtypeParse (RT3 Sltu)  >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Sltu r1 r2 r3)))
  , rtypeParse (RT3 Srlv)  >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Srlv r1 r2 r3)))
  , rtypeParse (RT3 Sub)   >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Sub  r1 r2 r3)))
  , rtypeParse (RT3 Subu)  >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Subu r1 r2 r3)))
  , rtypeParse (RT3 Xor)   >> (parseRegsN 3 >>= (\[r1, r2, r3] -> return (RI3  Xor  r1 r2 r3)))

  , rtypeParse (RT2 Div)   >> (parseRegsN 2 >>= (\[r1, r2] -> return (RI2 Div   r1 r2)))
  , rtypeParse (RT2 Divu)  >> (parseRegsN 2 >>= (\[r1, r2] -> return (RI2 Divu  r1 r2)))
  , rtypeParse (RT2 Mult)  >> (parseRegsN 2 >>= (\[r1, r2] -> return (RI2 Mult  r1 r2)))
  , rtypeParse (RT2 Multu) >> (parseRegsN 2 >>= (\[r1, r2] -> return (RI2 Multu r1 r2)))

  , rtypeParse (RT1 Mfhi)  >> (parseRegsN 1 >>= (\[r1] -> return (RI1 Mfhi r1)))
  , rtypeParse (RT1 Mflo)  >> (parseRegsN 1 >>= (\[r1] -> return (RI1 Mflo r1)))
  ]

parseRegsN :: (TokenParsing m, Monad m) => Int -> m [Reg]
parseRegsN n =
    -- foldl :: (m [Reg] -> m Reg -> m [Reg]) -> m [Reg] -> [m Reg] -> m [Reg]
    foldl cons' (return []) (replicate n register)
  where
      cons' bs b = do
          bs' <- bs
          b' <- b
          return (b':bs')
