{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reader
  ( reader
  ) where

import           Data.Maybe (catMaybes)
import qualified Data.Text    as T
import qualified Data.Text.IO as Tio
import           System.IO

data UserRole
  = Admin
  | Pleb
  deriving (Show)

userRoleFromText :: T.Text -> Maybe UserRole
userRoleFromText "pleb"  = Just Pleb -- can I make this "smarter" with case insensitivity?
userRoleFromText "admin" = Just Admin
userRoleFromText _       = Nothing

data User = User
  { first    :: T.Text
  , last     :: T.Text
  , password :: T.Text
  , role     :: UserRole
  } deriving (Show)

reader :: FilePath -> IO [User]
reader filepath = do
  file <- Tio.readFile filepath -- one big Text
  let rows = T.lines file -- split into rows as [Text]
  let users = catMaybes $ usermaker <$> rows
  return users

usermaker :: T.Text -> Maybe User
usermaker row = do
  case T.splitOn "," row of
    [firstName, lastName, roleName, pw] ->
      User firstName lastName pw <$> userRoleFromText roleName
    _ ->
      Nothing
