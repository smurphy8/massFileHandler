{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module System.MassFile where

import ClassyPrelude

import System.MassFile.Internal
import Filesystem
import Filesystem.Path.CurrentOS hiding (append)
import System.Locale
import Data.Time

data MFcfg = MFcfg { mfName :: FilePath,
                     mfCmd  :: Command,
                     mfOldDate :: UTCTime,
                     mfNewDate :: UTCTime
                   }
           deriving (Eq,Show)


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
      

  

timeFMT = "%F" -- yyyy-mm-dd

testTime :: String-> (Either Text UTCTime)
testTime  s = case parseTime defaultTimeLocale timeFMT s of 
                Just t ->  Right t 
                Nothing -> Left "invalid time should be yyyy-mm-dd"