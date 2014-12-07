{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Test.Hspec
import qualified Data.Vector as V
import GHC.Exts
import qualified Data.Sequence as Seq

import Data.Suffix

instance IsList (Seq.Seq a) where
    type Item (Seq.Seq a) = a
    fromList = Seq.fromList
    toList = toList

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Suffix" $ do
    classifyLSSpec
    lmsSubsSpec
    induceSortSpec


classifyLSSpec :: Spec
classifyLSSpec = describe "classifyLS" $ do
    it "should get the paper's example correct" $ do
        let ans =  [LTyp, LTyp, STyp, STyp, LTyp, LTyp, STyp, STyp, LTyp, LTyp, STyp, STyp, LTyp, LTyp, LTyp, LTyp, STyp]
        classifyLS eg `shouldBe` ans


lmsSubsSpec :: Spec
lmsSubsSpec = describe "lmsSubs" $ do
    it "should get the paper's example correct" $ do
        let ans = [ NotLMS, NotLMS, LMS 2, NotLMS, NotLMS, NotLMS , LMS 6
                  , NotLMS, NotLMS, NotLMS, LMS 10 , NotLMS, NotLMS,NotLMS
                  ,NotLMS,NotLMS, LMS 16 ]
        lmsSubs (classifyLS eg) `shouldBe` ans
    it "should always have the LMS constructor's argument be the elements index"
        pending
    it "should always have the last element be LMS"
        pending

induceSortSpec :: Spec
induceSortSpec = describe "induceSort" $ do
    it "should the paper's example correct" $ do
        let ans = [16, -1, -1, -1, -1, -1, 10, 06, 02, -1, -1, -1, -1, -1, -1, -1, -1]
        let val = induceSort eg (lmsSubs $ classifyLS eg)
        fst val `shouldBe` ans


