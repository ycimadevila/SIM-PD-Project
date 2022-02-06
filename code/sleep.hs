module Sleep where

import Control.Monad
import Data.Time
import System.IO

sleep total = do 
  t1 <- getCurrentTime
  sleep' t1 total

sleep' t1 total = do
       t2 <- getCurrentTime
       let time = ( realToFrac $ diffUTCTime t2 t1 :: Float) 
       if total > time then sleep' t1 total 
                       else return ()