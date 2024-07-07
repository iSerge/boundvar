{-# LANGUAGE BinaryLiterals #-}

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Um

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basicOperations]

haltOp :: Platter
haltOp = 0x70000000

ortho0Op :: Platter
ortho0Op = 0xD000000F

haltProg :: Program
haltProg = V.singleton haltOp

ortho0Prog :: Program
ortho0Prog = V.fromList [ortho0Op, haltOp]

basicOperations :: TestTree
basicOperations =
  testGroup
    "Test base operations"
    [ testCase "Halt" $ do
        result <- compute haltProg
        result @?= Right ("Evaluation halted", initUm haltProg),
      testCase "Orthography" $ do
        result <- compute ortho0Prog
        let startUm = initUm ortho0Prog
        let state = startUm { r = r startUm V.// [(0, 0x0000000F)], finger = 1}
        result @?= Right ("Evaluation halted", state)
    ]
