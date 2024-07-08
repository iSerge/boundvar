{-# LANGUAGE BinaryLiterals #-}

module Um (compute, initUm, Memory, nullUm, opN, Platter, Program, regA, regB, regC, Um (..)) where

import Control.Monad (unless, when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as ST
import Data.Bits (complement, shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Word (Word32, Word8)
import System.IO (stdin, stdout)

type Platter = Word32

type Program = V.Vector Platter

type Registers = V.Vector Platter

type Memory = M.Map Platter Program

data Um = Um {r :: Registers, mem :: Memory, finger :: Platter, opName :: String}
  deriving (Eq, Show)

type ComputeState = ST.StateT Um (E.ExceptT String IO)

nullUm :: Um
nullUm = Um {r = V.replicate 8 0, finger = 0, mem = M.empty, opName = ""}

initUm :: Program -> Um
initUm d = nullUm {mem = M.singleton 0 d}

getFinger :: ComputeState Int
getFinger = ST.gets $ fromIntegral . finger

advanceFinger :: ComputeState ()
advanceFinger = ST.modify (\s -> s {finger = finger s + 1})

getOp :: ComputeState Platter
getOp = do
  f <- getFinger
  s <- ST.get
  let m = mem s
  if M.member 0 m
    then
      if V.length (m M.! 0) <= f
        then do
          n <- getOpName
          E.throwError $ "Finger aims outside 0 array in " ++ n
        else return $ m M.! 0 V.! f
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
  unless (fromIntegral i < V.length (m M.! arr)) $ do
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
    -- Conditional move
    0 -> do
      setOpName "Conditional move"
      ST.modify $ condMove a b c
      advanceFinger
      evalStep
    -- Array index
    1 -> do
      setOpName "Array index"
      _ <- checkArray b
      checkArrayBound b c
      ST.modify $ arrayIndex a b c
      advanceFinger
      evalStep
    -- Array amendment
    2 -> do
      setOpName "Array amendment"
      _ <- checkArray a
      checkArrayBound a b
      ST.modify $ arrayAmend a b c
      advanceFinger
      evalStep
    -- Addition
    3 -> do
      setOpName "Addition"
      ST.modify $ add a b c
      advanceFinger
      evalStep
    -- Multiplication
    4 -> do
      setOpName "Multiplication"
      ST.modify $ mul a b c
      advanceFinger
      evalStep
    -- Division
    5 -> do
      setOpName "Division"
      divisor <- getReg c
      if 0 == divisor
        then do
          n <- getOpName
          E.throwError $ "Division by zero in " ++ n
        else ST.modify $ divide a b c
      advanceFinger
      evalStep
    -- Not-And
    6 -> do
      setOpName "Not-And"
      ST.modify $ nand a b c
      advanceFinger
      evalStep
    -- Halt
    7 -> do
      setOpName "Halt"
      return "Evaluation halted"
    -- Allocation
    8 -> do
      setOpName "Allocation"
      ST.modify $ allocate b c
      advanceFinger
      evalStep
    -- Abandonment
    9 -> do
      setOpName "Abandonment"
      i <- getReg c
      when (0 == i) $ do
        n <- getOpName
        E.throwError $ "Trying abandon " ++ show i ++ " array in " ++ n
      _ <- checkArray c
      ST.modify $ abandon c
      advanceFinger
      evalStep
    -- Output
    10 -> do
      setOpName "Output"
      v <- getReg c
      if v > 255
        then do
          n <- getOpName
          E.throwError $ "Trying to output value > 255 in " ++ n
        else ST.liftIO $ B.hPut stdout $ toByteString v
      advanceFinger
      evalStep
    -- Input
    11 -> do
      setOpName "Input"
      char <- ST.liftIO $ B.hGet stdin 1
      let v = if B.null char then complement (0 :: Platter) else fromIntegral $ B.head char
      ST.modify $ \s -> s {r = r s V.// [(c, v)]}
      advanceFinger
      evalStep
    -- Load program
    12 -> do
      setOpName "Load program"
      _ <- checkArray b
      ST.modify $ loadProg b c
      evalStep
    -- Orthography
    13 -> do
      setOpName "Orthography"
      let reg = fromIntegral $ 0b111 .&. (op `shiftR` 25)
      let v = 0x01FFFFFF .&. op
      ST.modify $ orthography reg v
      advanceFinger
      evalStep
    _ -> E.throwError $ "Failure: Invalid operation: " ++ show op

condMove :: Int -> Int -> Int -> Um -> Um
condMove a b c m = if 0 == r m V.! c then m else m {r = r m V.// [(a, r m V.! b)]}

arrayIndex :: Int -> Int -> Int -> Um -> Um
arrayIndex a b c m = m {r = r m V.// [(a, mem m M.! index V.! fromIntegral offset)]}
  where
    index = r m V.! b
    offset = r m V.! c

arrayAmend :: Int -> Int -> Int -> Um -> Um
arrayAmend a b c m = m {mem = M.adjust newVal index (mem m)}
  where
    index = r m V.! a
    offset = r m V.! b
    value = r m V.! c
    newVal _ = mem m M.! index V.// [(fromIntegral offset, value)]

add :: Int -> Int -> Int -> Um -> Um
add a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 + v2

mul :: Int -> Int -> Int -> Um -> Um
mul a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 * v2

divide :: Int -> Int -> Int -> Um -> Um
divide a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = v1 `div` v2

nand :: Int -> Int -> Int -> Um -> Um
nand a b c m = m {r = r m V.// [(a, value)]}
  where
    v1 = r m V.! b
    v2 = r m V.! c
    value = complement $ v1 .&. v2

allocate :: Int -> Int -> Um -> Um
allocate b c m = m {r = r m V.// [(b, index)], mem = M.insert index a memory}
  where
    count = fromIntegral $ r m V.! c
    a = if count == 0 then V.empty else V.replicate count 0
    memory = mem m
    index = head $ dropWhile (`M.member` memory) [1 ..]

abandon :: Int -> Um -> Um
abandon c m = m {mem = M.delete index $ mem m}
  where
    index = r m V.! c

loadProg :: Int -> Int -> Um -> Um
loadProg b c m = m {finger = r m V.! c, mem = M.adjust copy 0 $ mem m}
  where
    index = r m V.! b
    copy _ = mem m M.! index

orthography :: Int -> Platter -> Um -> Um
orthography a v m = m {r = r m V.// [(a, v)]}
