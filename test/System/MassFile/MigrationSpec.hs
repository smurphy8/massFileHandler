{-# LANGUAGE OverloadedStrings #-}
module System.MassFileSpec (main, spec) where
import Test.Hspec
import Test.QuickCheck
import Data.Time
import System.MassFile.Migration
main :: IO ()
main = hspec spec


dayStub :: Integer -> UTCTime 
dayStub n = (UTCTime (ModifiedJulianDay n) (fromIntegral n))



nextDayStub :: UTCTime 
nextDayStub = dayStub 1 

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
       testTime "1858-11-17" `shouldBe` (Right $ dayStub 0)




{-|



