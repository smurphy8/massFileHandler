module System.MassFile where

-- import System.MassFile.Internal

import Filesystem
import Filesystem.Path.CurrentOS

test :: IO () 
test = do 
  let p = decodeString "/home/scott/"
  isDirectory  p >>= print
  return () 