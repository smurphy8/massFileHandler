{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module System.MassFile where

import ClassyPrelude

import System.MassFile.Internal
import Filesystem
import Filesystem.Path.CurrentOS hiding (append)
import System.Locale
import Data.Time
import Control.Lens


newtype OldTime = OldTime {unOldTIme :: UTCTime}
    deriving (Eq,Show)

newtype NewTime = NewTime {unNewTime :: UTCTime }
    deriving (Eq,Show)

newtype TestTime = TestTime {unTestTime :: UTCTime}
    deriving (Eq,Show)

data MFcfg = MFcfg { mfName :: FilePath,
                     mfCmd  :: Command,
                     mfOldDate :: OldTime,
                     mfNewDate :: NewTime
                   }
           deriving (Eq,Show)
lensMfName :: (Functor f) => (FilePath -> f FilePath) -> MFcfg -> f (MFcfg)
lensMfName f (MFcfg a b c d) = fmap (\a' -> MFcfg a' b c d) (f a)

makeFileHandler :: MFcfg -> IO () 
makeFileHandler cfg = do 
  dirList <- listDirectory.mfName $ cfg
  mapM_ (\b -> return b) dirList
  return () 


-- testAndRunCMD :: MFcfg -> FilePath -> IO () 
-- testAndRunCMD cfg fp = do

--   case dirTruth 

testFilepath :: Text -> IO (Either Text FilePath)
testFilepath t = do
  p <- return $ (fromText $ t)
  case valid p of 
    True -> do 
      tst <- isDirectory p
      case tst of 
        False -> return $ Left $ append t "is not a File Path"
        True  -> return $ Right p
    False -> return $ Left $ append "Invalid File Path" t
      



makeRangeFilter :: OldTime -> NewTime -> (TestTime -> Bool)
makeRangeFilter (OldTime o) (NewTime n) (TestTime t) 
    | (o > t)   = False
    | (n <= t)  = False 
    | otherwise = True



timeFMT = "%F" -- yyyy-mm-dd


testTime :: String-> (Either Text UTCTime)
testTime  s = case parseTime defaultTimeLocale timeFMT s of 
                Just t ->  Right t 
                Nothing -> Left "invalid time should be yyyy-mm-dd"


-- | Make sure a time range is valid

testTimeRange :: OldTime -> NewTime -> Bool
testTimeRange (OldTime o) (NewTime n ) = o < n


  