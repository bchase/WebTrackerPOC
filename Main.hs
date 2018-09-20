{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ActionM, get, liftAndCatchIO, param, raise, scotty, text)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Hasql.Pool (acquire, Pool, release, use)
import Hasql.Session (Session)

import DBHelpers 

raiseError :: Show a => String a -> ActionM b
raiseError kind error = raise $ pack $ mconcat ["There was a ", kind, " error: ", show error]

dbSessionHandler :: Pool -> Session a -> ActionM a
dbSessionHandler pool session = let dbAction = use pool session
                                    scottyAction = liftAndCatchIO dbAction
				    eitherEorAToActionMA = either (raiseError "database connection pool") (return)
				in fmap eitherEorAToActionMA ScottyAction


main = do
  connections <- acquire settings
  let dbSessionHandler = let dbErrorToScottyError = either
                                                      (raise . pack . ("There was an error using the database connection pool: " <> ) . show)
                                                      (return)
		             
    ) . liftAndCatchIO . (use connections)
  scotty 3000 $ do
    get "/" $ homepage dbSessionHandler
    put "/login" $ handleLogin dbSessionHandler
    post "/consume" $ noteConsumption dbSessionHandler
  release connections -- should we do this? 

