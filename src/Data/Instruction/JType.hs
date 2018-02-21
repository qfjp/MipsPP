module Data.Instruction.JType where

import           Padelude                hiding (show, try)

import           Text.Show               (show)

import           Text.Parser.Combinators (choice, count)
import           Text.Parser.Token       (TokenParsing)

import           Data.Register
import           Parser.Utils

data JInstr =
     JI3 JText3 Text Reg Reg
   | JI2 JText2 Text Reg
   | JI1 JText1 Text
   | JIR JTextR Reg
   deriving (Show, Eq, Ord)

data JText =
     JT3 JText3
   | JT2 JText2
   | JT1 JText1
   | JTR JTextR
   deriving (Eq, Ord)

data JText3 =
    Beq | Bne
  deriving (Show, Eq, Ord, Enum)

data JText2 =
    Bgez | Bgezal | Bgtz | Blez | Bltz | Bltzal
  deriving (Show, Eq, Ord, Enum)

data JText1 =
    J | Jal
  deriving (Show, Eq, Ord, Enum)

data JTextR = Jr
  deriving (Show, Eq, Ord, Enum)

instance Show JText where
    show (JT3 r) = show r
    show (JT2 r) = show r
    show (JT1 r) = show r
    show (JTR r) = show r

jtype :: (TokenParsing m, Monad m) => m JInstr
jtype = choice . map makeJtype $
  [ JT3 Beq
  , JT3 Bne

  , JT2 Bgezal
  , JT2 Bgez
  , JT2 Bgtz
  , JT2 Blez
  , JT2 Bltzal
  , JT2 Bltz


  , JT1 Jal
  , JTR Jr
  , JT1 J
  ]

makeJtype :: (TokenParsing m, Monad m) => JText -> m JInstr
makeJtype i@(JT3 _) = makeJtypeP (parseArgsN 2) i
makeJtype i@(JT2 _) = makeJtypeP (parseArgsN 1) i
makeJtype i@(JT1 _) = makeJtypeP (parseArgsN 0) i
makeJtype i@(JTR _) = makeJtypeP (parseJrArgs) i

makeJtypeP :: (TokenParsing m, Monad m) => m (Text, [Reg]) -> JText -> m JInstr
makeJtypeP parse i = do
    void $ parseDataAsShow i
    (lab, regs) <- parse
    let maybInstr = regsToJInstr i regs lab
    case maybInstr of
      Just x  -> return x
      Nothing -> undefined

regsToJInstr :: JText -> [Reg] -> Text -> Maybe JInstr
regsToJInstr (JT3 instr) [r1, r2] lab = Just $
    JI3 instr lab r1 r2
regsToJInstr (JT2 instr) [r] lab = Just $
    JI2 instr lab r
regsToJInstr (JT1 instr) _ lab = Just $
    JI1 instr lab
regsToJInstr (JTR instr) [r] _ = Just $
    JIR instr r
regsToJInstr _ _ _ = Nothing

parseArgsN :: (TokenParsing m, Monad m) => Int -> m (Text, [Reg])
parseArgsN n = do
    regs <- count n register
    lab <- label
    return (lab, regs)

parseJrArgs :: (TokenParsing m, Monad m) => m (Text, [Reg])
parseJrArgs = do
    reg <- register
    return ("", [reg])
