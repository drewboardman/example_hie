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
  | Pleb deriving (Show)

userRoleFromText :: T.Text -> Maybe UserRole
userRoleFromText "pleb" = Just Pleb -- can I make this "smarter" with case insensitivity?
userRoleFromText "admin" = Just Admin
userRoleFromText _ = Nothing

data User = User
  { first    :: T.Text
  , last     :: T.Text
  , role     :: UserRole
  , password :: T.Text
  } deriving (Show)

reader :: FilePath -> IO [User]
reader filepath = do
  file <- Tio.readFile filepath -- one big Text
  let rows = T.lines file -- split into rows as [Text]
  let users = [x | Just x <- map usermaker rows]
  return users

usermaker :: T.Text -> Maybe User
usermaker row = user where
  split = T.splitOn "," row -- how do I just put these into the constructor?
  first' = split !! 0
  last' = split !! 1
  role' = userRoleFromText $ split !! 2
  password' = split !! 3
  user =  case role' of
    Nothing -> Nothing
    Just realRole -> Just $ User first' last' realRole password' -- I'd rather just map over role. How to?
