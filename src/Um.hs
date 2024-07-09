{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}

module Um (compute, initUm, Memory, nullUm, opN, Platter, Program, regA, regB, regC, Um (..)) where

import Control.Monad (unless, when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as ST
import qualified Data.Array.IO as A
import Data.Bits (complement, shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Word (Word32, Word8)
import System.IO (hFlush, stdin, stdout)

type Platter = Word32

type Program = A.IOUArray Platter Platter

type Registers = V.Vector Platter

type Memory = M.Map Platter Program

data Um = Um {r :: Registers, mem :: Memory, finger :: Platter, opName :: String}
  deriving (Eq)

-- deriving (Eq, Show)

type ComputeState = ST.StateT Um (E.ExceptT String IO)

nullUm :: Um
nullUm = Um {r = V.replicate 8 0, finger = 0, mem = M.empty, opName = ""}

initUm :: Program -> Um
initUm d = nullUm {mem = M.singleton 0 d}

getFinger :: ComputeState Platter
getFinger = ST.gets finger

advanceFinger :: ComputeState ()
advanceFinger = ST.modify (\s -> s {finger = finger s + 1})

progLen :: Program -> IO Platter
progLen p = do
  (_, end) <- A.getBounds p
  return end

getOp :: ComputeState Platter
getOp = do
  f <- getFinger
  s <- ST.get
  let m = mem s
  if M.member 0 m
    then do
      l <- ST.liftIO $ progLen (m M.! 0)
      if l <= f
        then do
          n <- getOpName
          E.throwError $ "Finger aims outside 0 array in " ++ n
        else ST.liftIO $ A.readArray (m M.! 0) f
    else do
      n <- getOpName
      E.throwError $ "0 array is absent in " ++ n

setOpName :: String -> ComputeState ()
setOpName n = ST.modify $ \s -> s {opName = n}

getOpName :: ComputeState String
getOpName = ST.gets $ \s -> opName s

opN :: Platter -> Platter
opN p = 0b1111 .&. shiftR p 28

regA :: Platter -> Int
regA p = fromIntegral $ 0b111 .&. shiftR p 6

regB :: Platter -> Int
regB p = fromIntegral $ 0b111 .&. shiftR p 3

regC :: Platter -> Int
regC p = fromIntegral $ 0b111 .&. p

compute :: Program -> IO (Either String (String, Um))
compute p = E.runExceptT $ ST.runStateT evalStep $ initUm p

toWord8 :: Platter -> Word8
toWord8 = fromIntegral

toByteString :: Platter -> B.ByteString
toByteString = B.singleton . toWord8

getMem :: ComputeState Memory
getMem = ST.gets $ \s -> mem s

getReg :: Int -> ComputeState Platter
getReg i = ST.gets $ \s -> r s V.! i

checkArray :: Int -> ComputeState Program
checkArray reg = do
  i <- getReg reg
  m <- getMem
  if M.member i m
    then return $ m M.! i
    else do
      n <- getOpName
      E.throwError $ "Array " ++ show i ++ " is absent in " ++ n

checkArrayBound :: Int -> Int -> ComputeState ()
checkArrayBound a reg = do
  i <- getReg reg
  arr <- getReg a
  m <- getMem
  l <- ST.liftIO $ progLen (m M.! arr)
  unless (i <= l) $ do
    n <- getOpName
    E.throwError $ "Index " ++ show i ++ " is out of bounds of array in " ++ n

evalStep :: ComputeState String
evalStep = do
  -- ST.liftIO $ putStrLn "Evaluating step"
  setOpName "Prepare op execution"
  op <- getOp
  let a = regA op
  let b = regB op
  let c = regC op
  case opN op of
    0 -> conditionalMove a b c
    1 -> arrayIndex a b c
    2 -> amendment a b c
    3 -> addition a b c
    4 -> multiplication a b c
    5 -> division a b c
    6 -> notAnd a b c
    7 -> halt
    8 -> allocation b c
    9 -> abandonment c
    10 -> output c
    11 -> input c
    12 -> loadProg b c
    13 -> orthography op
    _ -> E.throwError $ "Failure: Invalid operation: " ++ show op

conditionalMove :: Int -> Int -> Int -> ComputeState String
conditionalMove a b c = do
  setOpName "Conditional move"
  ST.modify $ condMove a b c
  advanceFinger
  evalStep

condMove :: Int -> Int -> Int -> Um -> Um
condMove a b c m = if 0 == r m V.! c then m else m {r = r m V.// [(a, r m V.! b)]}

arrayIndex :: Int -> Int -> Int -> ComputeState String
arrayIndex a b c = do
  setOpName "Array index"
  m <- checkArray b
  checkArrayBound b c
  -- ST.modify $ arrayIndex a b c
  ofs <- getReg c
  v <- ST.liftIO $ A.readArray m ofs
  ST.modify $ \s -> s {r = r s V.// [(a, v)]}
  advanceFinger
  evalStep

amendment :: Int -> Int -> Int -> ComputeState String
amendment a b c = do
  setOpName "Array amendment"
  m <- checkArray a
  checkArrayBound a b
  ofs <- getReg b
  v <- getReg c
  ST.liftIO $ A.writeArray m ofs v
  advanceFinger
  evalStep

addition :: Int -> Int -> Int -> ComputeState String
addition a b c = do
  setOpName "Addition"
  ST.modify $ add a b c
  advanceFinger
  evalStep

add :: Int -> Int -> Int -> Um -> Um
add a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 + v2

multiplication :: Int -> Int -> Int -> ComputeState String
multiplication a b c = do
  setOpName "Multiplication"
  ST.modify $ mul a b c
  advanceFinger
  evalStep

mul :: Int -> Int -> Int -> Um -> Um
mul a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 * v2

division :: Int -> Int -> Int -> ComputeState String
division a b c = do
  setOpName "Division"
  divisor <- getReg c
  if 0 == divisor
    then do
      n <- getOpName
      E.throwError $ "Division by zero in " ++ n
    else ST.modify $ divide a b c
  advanceFinger
  evalStep

divide :: Int -> Int -> Int -> Um -> Um
divide a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 `div` v2

notAnd :: Int -> Int -> Int -> ComputeState String
notAnd a b c = do
  setOpName "Not-And"
  ST.modify $ nand a b c
  advanceFinger
  evalStep

nand :: Int -> Int -> Int -> Um -> Um
nand a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = complement $ v1 .&. v2

halt :: ComputeState String
halt = do
  setOpName "Halt"
  return "Evaluation halted"

allocation :: Int -> Int -> ComputeState String
allocation b c = do
  setOpName "Allocation"
  sz <- getReg c
  memory <- ST.gets mem
  arr <- if sz == 0 then ST.liftIO $ A.newArray (0, 0) 0 else ST.liftIO $ A.newArray (0, sz - 1) 0
  let index = head $ dropWhile (`M.member` memory) [1 ..]
  ST.modify $ \s -> s {r = r s V.// [(b, index)], mem = M.insert index arr memory}
  advanceFinger
  evalStep

abandonment :: Int -> ComputeState String
abandonment c = do
  setOpName "Abandonment"
  i <- getReg c
  when (0 == i) $ do
    n <- getOpName
    E.throwError $ "Trying abandon " ++ show i ++ " array in " ++ n
  _ <- checkArray c
  ST.modify $ \m -> m {mem = M.delete (r m V.! c) $ mem m}
  advanceFinger
  evalStep

output :: Int -> ComputeState String
output c = do
  setOpName "Output"
  v <- getReg c
  if v > 255
    then do
      n <- getOpName
      E.throwError $ "Trying to output value > 255 in " ++ n
    else do
      ST.liftIO $ B.hPut stdout $ toByteString v
      ST.liftIO $ hFlush stdout
  advanceFinger
  evalStep

input :: Int -> ComputeState String
input c = do
  setOpName "Input"
  char <- ST.liftIO $ B.hGet stdin 1
  let v = if B.null char then complement (0 :: Platter) else fromIntegral $ B.head char
  ST.modify $ \s -> s {r = r s V.// [(c, v)]}
  advanceFinger
  evalStep

loadProg :: Int -> Int -> ComputeState String
loadProg b c = do
  setOpName "Load program"
  offs <- getReg c
  n <- getReg b
  if n == 0
    then ST.modify $ \s -> s {finger = offs}
    else do
      arr <- checkArray b
      bytes <- ST.liftIO $ A.getElems arr
      bounds <- ST.liftIO $ A.getBounds arr
      prog <- ST.liftIO $ A.newListArray bounds bytes
      ST.modify $ \s -> s {finger = offs, mem = M.adjust (const prog) 0 $ mem s}
  evalStep

orthography :: Platter -> ComputeState String
orthography op = do
  setOpName "Orthography"
  let reg = fromIntegral $ 0b111 .&. (op `shiftR` 25)
  let v = 0x01FFFFFF .&. op
  ST.modify $ \m -> m {r = r m V.// [(reg, v)]}
  advanceFinger
  evalStep
