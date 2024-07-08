{-# LANGUAGE BinaryLiterals #-}

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Um

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basicOperations]

basicOperations :: TestTree
basicOperations =
  testGroup
    "Test base operations"
    [ testCase "Halt" $ do
        result <- compute haltProg
        result @?= Right ("Evaluation halted", (initUm haltProg) {opName = "Halt"}),
      testCase "Orthography" $ do
        result <- compute ortho0Prog
        let startUm = initUm ortho0Prog
        let state = startUm {r = r startUm V.// [(0, 0x0000000F)], finger = 1, opName = "Halt"}
        result @?= Right ("Evaluation halted", state),
      testCase "Orthography 2" $ do
        result <- compute ortho0Prog2
        let startUm = initUm ortho0Prog2
        let state = startUm {r = r startUm V.// [(1, 0x0100000F)], finger = 1, opName = "Halt"}
        result @?= Right ("Evaluation halted", state),
      testCase "Orthography 3" $ do
        result <- compute ortho0Prog3
        let startUm = initUm ortho0Prog3
        let state = startUm {r = r startUm V.// [(7, 0x0100000F)], finger = 1, opName = "Halt"}
        result @?= Right ("Evaluation halted", state)
    ]

haltOp :: Platter
haltOp = 0x70000000

ortho0Op :: Platter
ortho0Op = 0xD000000F

ortho0Op2 :: Platter
ortho0Op2 = 0xD300000F

ortho0Op3 :: Platter
ortho0Op3 = 0xDF00000F

haltProg :: Program
haltProg = V.singleton haltOp

ortho0Prog :: Program
ortho0Prog = V.fromList [ortho0Op, haltOp]

ortho0Prog2 :: Program
ortho0Prog2 = V.fromList [ortho0Op2, haltOp]

ortho0Prog3 :: Program
ortho0Prog3 = V.fromList [ortho0Op3, haltOp]
