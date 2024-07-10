module Main (main) where

import qualified Data.Array.IO as A
import Data.Bits (shiftL)
import qualified Data.ByteString as B
import Data.List (intersperse)
import System.Environment (getArgs)
import Um

bytesToProgram :: B.ByteString -> IO Program
bytesToProgram s = A.newListArray (0, fromIntegral $ B.length s - 1) (toWord32 s)
  where
    toWord32 bs | B.null bs = []
    toWord32 bs =
      let (a, b) = B.splitAt 4 bs
          w = B.foldl' (\acc byte -> shiftL acc 8 + fromIntegral byte :: Platter) 0 a
       in w : toWord32 b

main :: IO ()
main = do
  args <- getArgs
  contents <- mapM B.readFile args
  prog <- bytesToProgram $ B.concat contents
  result <- compute prog
  case result of
    Left s -> do
      putStrLn ""
      putStrLn "-------------------"
      putStr "Failure: "
      putStrLn s
    Right _ -> return ()
