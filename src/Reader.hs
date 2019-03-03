{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables          #-}

module Reader (reader) where

import qualified Data.Text as T
import System.IO

data UserRole = Admin | Pleb

data User = User
  { first :: String
  , last :: String
  , role :: UserRole
  , password :: String
  }


reader :: FilePath -> IO String
reader filepath = do
  file <- (readFile filepath :: IO String)
  let users = (lines file :: [String])
  undefined

usersmaker :: T.Text -> [User]
usersmaker row = do
  _splitted <- T.splitOn "," row
  return $ User "foo" "bar" Pleb "password"

usermaker2 :: T.Text -> User
usermaker2 row = user where
  x = T.splitOn "," row
  user = User "foo" "bar" Pleb "password"
