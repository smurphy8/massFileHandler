{-# LANGUAGE OverloadedStrings #-}
module System.MassFileSpec (main, spec) where
import Test.Hspec
import Test.QuickCheck
import Data.Time
import System.MassFile
main :: IO ()
main = hspec spec


dayStub :: Integer -> UTCTime 
dayStub n = (UTCTime (ModifiedJulianDay n) (fromIntegral n))



nextDayStub :: UTCTime 
nextDayStub = dayStub 1 

spec :: Spec
spec = do
  describe "testTime" $ do
    it "should decode yyyy-mm-dd to valid UTC" $ do
       testTime "1858-11-17" `shouldBe` (Right $ dayStub 0)
  describe "testTimeRange" $ do 
    it "should return true if an old UTC time is greater than a new one" $ do
         (testTimeRange (OldTime $ dayStub 0) (NewTime $ nextDayStub )) `shouldBe` True
  describe "makeRangeFilter" $ do 
    it "should return true if test time is between old and new" $ do 
     makeRangeFilter (OldTime $ dayStub 0) (NewTime $ dayStub 3 ) (TestTime $ dayStub 2) `shouldBe` True
     makeRangeFilter (OldTime $ dayStub 1) (NewTime $ dayStub 3 ) (TestTime $ dayStub 0) `shouldBe` False
     makeRangeFilter (OldTime $ dayStub 1) (NewTime $ dayStub 3 ) (TestTime $ dayStub 4) `shouldBe` False
  -- describe "lensMfName" $ do 
  --   it "should get back what is put in" $ do 
  --                           (lensMfName 

