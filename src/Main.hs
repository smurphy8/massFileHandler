{-# LANGUAGE NoImplicitPrelude,OverloadedStrings,DeriveDataTypeable #-}
module Main where
import ClassyPrelude
import System.MassFile.Internal
import System.MassFile

main :: IO () 
main = do 
  (MassFileCmdArgs f c o n ) <- runCmdArgs
  eFP <- testFilepath.pack $ f
  let eTO = OldTime <$> testTime o
      eTN = NewTime <$> testTime n 
  print eTO 
  print eTN
  print eFP
  return () 