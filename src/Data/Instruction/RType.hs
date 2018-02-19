module Data.Instruction.RType where

import           Padelude                hiding (show, try)

import           Data.List.NonEmpty      (fromList)
import           Data.Text               (pack, toLower)
import           Text.Show               (show)

import           Data.Either.Extra       (fromRight')

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
rtype = choice . map ((try $) . makeRtype) $
  [ RT3 Add
  , RT3 Addu
  , RT3 And
  , RT3 Or
  , RT3 Sllv
  , RT3 Slt
  , RT3 Sltu
  , RT3 Srlv
  , RT3 Sub
  , RT3 Subu
  , RT3 Xor

  , RT2 Div
  , RT2 Divu
  , RT2 Mult
  , RT2 Multu

  , RT1 Mfhi
  , RT1 Mflo
  ]

makeRtype :: (TokenParsing m, Monad m) => RText -> m RInstr
makeRtype i@(RT3 _) = makeRtypeN 3 i
makeRtype i@(RT2 _) = makeRtypeN 2 i
makeRtype i@(RT1 _) = makeRtypeN 1 i

makeRtypeN :: (TokenParsing m, Monad m) => Int -> RText -> m RInstr
makeRtypeN n i =
    rtypeParse i >> parseRegsN n >>= (return . regsToRInstr i . fromList)

-- [1, 2, 3] => f $ 1 $ 2 $ 3
type PAPFunc f a result = Either (f, [a]) result

liftPartial f xs = Left (f,xs)

apply (Left (f,xs)) ys = Left (f,xs++ys)
apply p _              = p

--simp2 :: PAPFunc (a->a->b) a b -> PAPFunc (a->a->b) a b
simp2 :: PAPFunc (a -> a -> b) a b -> PAPFunc (a -> a -> b) a b
simp2 (Left (f,(x1:x2:xs))) = Right (f x1 x2)
simp2 p                     = p

unsafeUnwrap :: PAPFunc (a -> a -> b) a b -> b
unsafeUnwrap = fromRight'

regsToRInstr :: RText -> NonEmpty Reg -> RInstr
regsToRInstr (RT3 i) (r:|rs) =
    unsafeUnwrap . simp2 $ liftPartial (RI3 i r) rs
regsToRInstr (RT2 i) (r:|rs) =
    unsafeUnwrap . simp2 $ liftPartial (RI2 i) (r:rs)
regsToRInstr (RT1 i) (r:|[]) =
    unsafeUnwrap . simp2 $ liftPartial (const $ RI1 i) [r, undefined]

parseRegsN :: (TokenParsing m, Monad m) => Int -> m [Reg]
parseRegsN n =
    foldl cons' (return []) (replicate n register)
  where
      cons' bs b = do
          bs' <- bs
          b' <- b
          return (b':bs')
