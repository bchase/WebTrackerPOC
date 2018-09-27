{-# LANGUAGE OverloadedStrings #-}

module AuthSessionHelpers where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text, toStrict)
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (Pool)
import Web.Scotty (ActionM, liftAndCatchIO)

import DBHelpers (scottyDoesDB)
import DBTypes (addRow)
import qualified DBTypes.Account as Account (Row(..))
import qualified DBTypes.AuthSession as AuthSession (Row(..))


makeNewUserSession :: Pool -> Text -> ActionM (AuthSession.Row, ByteString)
makeNewUserSession connections newUsername = undefined -- do
--  account <- makeNewUser connections newUsername
  

makeSessionForUser :: Pool -> Account.Row -> ActionM (AuthSession.Row, ByteString)
makeSessionForUser connections account = undefined

makeNewUser :: Pool -> Text -> ActionM Account.Row
makeNewUser connections username = do
  newID <- liftAndCatchIO nextRandom
  let row = Account.Row {
    Account.identifier = newID,
    Account.name = toStrict username
    }
  scottyDoesDB connections $ addRow row
  return row

