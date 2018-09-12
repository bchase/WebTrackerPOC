{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Hasql.Session
import Hasql.Statement

main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    let phrase = mconcat ["Scotty, ", beam, " me up!"]
    liftAndCatchIO $ print phrase
    text phrase

