{-# LANGUAGE NoImplicitPrelude,OverloadedStrings,DeriveDataTypeable #-}

module System.MassFile.Internal
    (Command (..),MassFileCmdArgs(..), runCmdArgs
    ) where

import System.Console.CmdArgs
import ClassyPrelude
import Data.String

data Command = List|Delete|Error
               deriving (Read,Eq,Show,Data,Typeable)

data MassFileCmdArgs = MassFileCmdArgs { rootFilename :: String ,
                                         command  :: Command,
                                         oldDate  :: String,
                                         newDate  :: String ,
                                         recursive :: Bool
                                         } deriving (Show, Data, Typeable)


dflt :: MassFileCmdArgs
dflt = MassFileCmdArgs { rootFilename = "",
                         command = Error,
       oldDate = "noOldDateGiven",
       newDate = "noNewDateGiven",
       recursive = True}
       
runCmdArgs = cmdArgs dflt