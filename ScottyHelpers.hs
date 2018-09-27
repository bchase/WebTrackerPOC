{-# LANGUAGE OverloadedStrings #-}

module ScottyHelpers where

import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, header, liftAndCatchIO)

getReferer :: ActionM (Maybe Text)
getReferer = header "Referer"

getTime :: ActionM (UTCTime)
getTime = liftAndCatchIO getCurrentTime
