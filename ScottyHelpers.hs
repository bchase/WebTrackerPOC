{-# LANGUAGE OverloadedStrings #-}

module ScottyHelpers where

import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.Text as Eager (Text)
import Data.Text.Lazy (Text, toStrict)
import Web.Scotty (ActionM, header, liftAndCatchIO)

getReferer :: ActionM (Maybe Eager.Text)
getReferer = do
  maybeLazy <- header "Referer"
  let maybeEager = fmap toStrict maybeLazy
  return maybeEager

getTime :: ActionM (UTCTime)
getTime = liftAndCatchIO getCurrentTime
