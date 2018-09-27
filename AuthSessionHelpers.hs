{-# LANGUAGE OverloadedStrings #-}

module AuthSessionHelpers where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Web.Scotty (ActionM)

import qualified DBTypes.Account as Account (Row(..))
import qualified DBTypes.AuthSession as AuthSession (Row(..))


makeNewUserSession :: Text -> ActionM (AuthSession.Row, ByteString)
makeNewUserSession newUsername = 

makeSessionForUser :: Account.Row -> ActionM (AuthSession.Row, ByteString)
makeSessionForUser account = 

makeNewUser :: Text -> Session Account.Row
makeNewUser username = do
  let row = Account.Row {
    Account.identifier = 
    }

