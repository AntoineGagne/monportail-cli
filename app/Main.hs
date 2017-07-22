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
import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TlsHttpClient
import qualified Network.Wreq as Wreq

import qualified Authentication
import qualified Calendar
import qualified Parser


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
    print response
    case Authentication.parseLoginDetails (response ^. Wreq.responseBody) of
        Left errors -> print errors
        Right loginDetails -> Calendar.fetchCalendarDetails manager loginDetails >>= print
