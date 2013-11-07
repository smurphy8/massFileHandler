{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module System.MassFile where

import ClassyPrelude

import System.MassFile.Internal
import Filesystem
import Filesystem.Path.CurrentOS hiding (append)
import System.Locale
import Data.Time
import Control.Lens
import Control.Monad hiding (mapM_)


type FileFcn a = FilePath -> IO a

newtype OldTime = OldTime {unOldTIme :: UTCTime}
    deriving (Eq,Show)

newtype NewTime = NewTime {unNewTime :: UTCTime }
    deriving (Eq,Show)

newtype TestTime = TestTime {unTestTime :: UTCTime}
    deriving (Eq,Show)

data MFcfg cmd = MFcfg  { mfName :: FilePath,
                          mfCmd  :: cmd,
                          mfOldDate :: OldTime,
                          mfNewDate :: NewTime
                   }



-- |More lens experiments
lensMfName :: Lens (MFcfg cmd) (MFcfg cmd) FilePath FilePath 
lensMfName f (MFcfg a b c d) = fmap (\a' -> MFcfg a' b c d) (f a)

lensMfCmd :: Lens (MFcfg cmd) (MFcfg cmd) cmd cmd
lensMfCmd f (MFcfg a cmdFcn c d) = fmap (\cmdFcn' -> MFcfg a cmdFcn' c d) (f cmdFcn)




pathTraverseHandler :: (MFcfg (FileFcn a)) -> IO () 
pathTraverseHandler cfg = do 
  dirContent <- listDirectory (mfName cfg) -- incoming root directory content
  dirlist <- filterM (filterDirsAndRunCMD cfg) dirContent :: IO [FilePath]
  mapM_ pathTraverseHandler ((set lensMfName) <$> dirlist <*> [cfg])
  return ()



-- | Tests to make sure the income file path is a valid File, 
--   runs the command on it and returns false to take it out of the
--   list!

filterDirsAndRunCMD :: (MFcfg (FileFcn a) ) -> FilePath -> IO Bool
filterDirsAndRunCMD cfg fp = do
    let rangeFilter = (makeRangeFilter (mfOldDate cfg) (mfNewDate cfg))
    ftest <- isFile fp 
    fDate <- getModified fp
    case ftest  of 
      True -> case (rangeFilter $ TestTime fDate) of 
                True -> (cfg ^. lensMfCmd) fp >> return False 
                False -> return False
      False -> return True -- Because it is a directory

    
--  let fp' = (cfg ^.lensMfName)  
--  return $  lensMfName .~ fp' $ cfg


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



-- | Everything about commands is premised on the idea they are terminal 
-- It is therefore appropriate to require a () 
commandAssembler :: Command -> FilePath -> IO ()
commandAssembler List = print 
commandAssembler Error = (\_ -> print "Something has gone wrong with your command")
commandAssembler Delete = removeFile 


-- | Take in a string, read it and see if it is a valid command If so return that as Either
-- If not do the other
testCommand :: Command -> Either Text (FileFcn ())
testCommand Error = Left $ "Something has gone wrong with your command"
testCommand x = Right $ commandAssembler x