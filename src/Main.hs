{-# LANGUAGE NoImplicitPrelude,OverloadedStrings,DeriveDataTypeable #-}
module Main where
import ClassyPrelude hiding (mapM)
import System.MassFile.Internal
import System.MassFile
import Data.Traversable
main :: IO () 
main = do 
  (MassFileCmdArgs f c o n ) <- runCmdArgs
  eFP <- testFilepath.pack $ f

  let eTO = OldTime <$> testTime o
      eTN = NewTime <$> testTime n 
      eCMD = testCommand c
      eCFG = MFcfg <$> eFP <*> eCMD <*> eTO <*> eTN
      delayedThing = (\_ -> print "done") :: Text -> xIO ()
  print eTO 
  print eTN
  print eFP
  print c
  
  either (delayedThing) pathTraverseHandler eCFG

 