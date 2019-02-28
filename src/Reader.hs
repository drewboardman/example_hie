module Reader (reader) where

import System.IO

reader :: FilePath -> IO String
reader filepath = do
  file <- openFile filepath ReadMode
