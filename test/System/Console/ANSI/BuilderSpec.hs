{-# LANGUAGE OverloadedStrings #-}
module System.Console.ANSI.BuilderSpec where

import           Data.Sequence
import           System.Console.ANSI
import           System.Console.ANSI.Builder
import           Test.Hspec

spec :: Spec
spec = do
    describe "ANSI helpers" $ do
        it "lets us easily construct Builder values" $
            vBlue <> "Hello"
                `shouldBe` Builder (fromList [("Hello", [SetColor Foreground Vivid Blue])])

        it "lets us easily compose SGR values" $
            vBlue <> vBlueBg <> "Hello"
                `shouldBe` Builder (fromList [("Hello", [SetColor Foreground Vivid Blue
                                                        ,SetColor Background Vivid Blue])])

    describe "fuse seq1 seq2" $ do
        it "appends two ANSITextSeqs" $
            fuse (fromList [("Hello", [SetColor Foreground Vivid Blue])])
              (fromList [(" world", [Reset])])
              `shouldBe` fromList [ ("Hello", [SetColor Foreground Vivid Blue])
                                  , (" world", [Reset])
                                  ]

        it "fuses empty SGR texts" $
            fuse (fromList [("Hello", [])])
              (fromList [(" world", [])])
              `shouldBe` fromList [ ("Hello world", [])
                                  ]

        it "fuses empty SGR texts correctly for long sequences" $
            fuse (fromList [ ("Hello", [SetColor Foreground Vivid Blue])
                           , (" wonderful", [SetConsoleIntensity BoldIntensity])
                           , (" beautiful", [])
                           ])
              (fromList [(" world", [])])
              `shouldBe` fromList [ ("Hello", [SetColor Foreground Vivid Blue])
                                  , (" wonderful", [SetConsoleIntensity BoldIntensity])
                                  , (" beautiful world", [])
                                  ]
