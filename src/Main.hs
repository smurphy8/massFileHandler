{-# LANGUAGE NoImplicitPrelude,OverloadedStrings,DeriveDataTypeable #-}
module Main where
import ClassyPrelude
import System.MassFile.Internal
import System.MassFile

main :: IO () 
main = do 
  (MassFileCmdArgs f c o n r) <- runCmdArgs
  eFP <- testFilepath.pack $ f
  eTO <- testTime o
  eTN <- testTime n 
  
  return () 