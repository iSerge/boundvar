module Main (main) where

import Data.Bits (shiftL)
import qualified Data.ByteString as B
import Data.List (intersperse)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Um

bytesToProgram :: B.ByteString -> Program
bytesToProgram = V.fromList . toWord32
  where
    toWord32 bs | B.null bs = []
    toWord32 bs =
      let (a, b) = B.splitAt 4 bs
          -- f :: Platter -> Word8 -> Platter
          w = B.foldl (\acc byte -> shiftL acc 8 + fromIntegral byte :: Platter) 0 a
       in w : toWord32 b

main :: IO ()
main = do
  args <- getArgs
  putStr "Got args: "
  mapM_ putStr $ intersperse ", " args
  putStrLn ""
  contents <- mapM (\f -> do putStr "Reading file: "; putStrLn f; B.readFile f) args
  putStrLn "-------------------"
  result <- compute $ bytesToProgram $ B.concat contents
  case result of
    Left s -> do
      putStrLn ""
      putStrLn "-------------------"
      putStr "Failure: "
      putStrLn s
    Right (s, _) -> do
      putStrLn ""
      putStrLn "-------------------"
      putStr "Succes: "
      putStrLn s
