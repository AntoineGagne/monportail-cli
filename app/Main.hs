{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception ( bracket_
                         )
import System.IO ( hFlush
                 , putStr
                 , putChar
                 , stdout
                 , stdin
                 , hSetEcho
                 , hGetEcho
                 )

import Control.Lens ( (^.) )

import qualified Data.Aeson as Aeson
import qualified Brick
import qualified Brick.Main as BrickMain
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TlsHttpClient
import qualified Network.Wreq as Wreq

import qualified Authentication
import qualified Calendar
import qualified Parser
import qualified UI


getUsername :: IO ByteString.ByteString
getUsername = getUserInput True "Username: "

getPassword :: IO ByteString.ByteString
getPassword = do
    password <- getUserInput False "Password: "
    putStr "\n" >> pure password

getUserInput :: Bool -> String -> IO ByteString.ByteString
getUserInput echo parameter = do
    putStr parameter
    hFlush stdout
    withEcho echo ByteString.getLine

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = do
    user <- Authentication.User <$> getUsername <*> getPassword
    manager <- HttpClient.newManager TlsHttpClient.tlsManagerSettings
    cookies <- Authentication.fetchAuthenticationCookies manager
    response <- Authentication.fetchCredentials user cookies
    currentTime <- Clock.getCurrentTime
    timezone <- Time.getCurrentTimeZone
    let localTime = Time.utcToLocalTime timezone currentTime
        endingTime = (Time.utcToLocalTime timezone . Time.addUTCTime diffTime) currentTime
    case Authentication.parseLoginDetails (response ^. Wreq.responseBody) of
        Left errors -> print errors
        Right loginDetails -> do
            calendar <- Calendar.fetchCalendarDetails manager loginDetails
            case calendar of
              Left errors' -> print errors'
              Right calendarDetails -> do
                  events <- Calendar.fetchEvents manager loginDetails calendarDetails localTime endingTime
                  case events of
                      Left errors'' -> print errors''
                      Right events' -> BrickMain.simpleMain (UI.displayEvents events' :: Brick.Widget Text.Text)
    where
        diffTime :: Time.NominalDiffTime
        diffTime = 10368000
