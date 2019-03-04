{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reader
  ( reader
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           System.IO

data UserRole
  = Admin
  | Pleb

data User = User
  { first    :: String
  , last     :: String
  , role     :: UserRole
  , password :: String
  }

reader :: FilePath -> IO [User]
reader filepath = do
  file <- Tio.readFile filepath -- one big Text
  let rows = T.lines file -- split into rows as [Text]
  let users = map usermaker rows
  undefined

usermaker :: T.Text -> User
usermaker row = do
  let split :: _ = T.splitOn "," row
  let user = User { first = T.unpack $ split!!0
                  , last = T.unpack $ split!!1
                  , role = T.unpack $ split!!2
                  , password = T.unpack $ split!!3 }
  return $ User "foo" "bar" Pleb "password"
