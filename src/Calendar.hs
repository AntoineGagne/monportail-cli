{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Calendar ( Calendar (..)
                , Metadata (..)
                , Event (..)
                , Events (..)
                , Communication (..)
                , extractTime
                , toULavalTime
                , fetchCalendarDetails
                ) where

import Data.Aeson ( FromJSON (..)
                  , ToJSON (..)
                  , (.:)
                  , (.:?)
                  )
import Data.Text ( Text )
import Network.HTTP.Client ( CookieJar
                           , Manager
                           , Response
                           , Request (..)
                           )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Data.Time.Format as TimeFormat
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TlsHttpClient
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as WreqSession
import qualified Network.Wreq.Types as WreqTypes

import qualified Authentication


data CalendarError = UnexpectedResponse (Maybe String)
    deriving (Show)


baseRoute :: String
baseRoute = "https://monportail.ulaval.ca"

fetchCalendarDetails :: Manager -> Authentication.LoginDetails -> IO (Either CalendarError Calendar)
fetchCalendarDetails manager loginDetails = do
    baseRequest <- HttpClient.setQueryString queryParameter <$> HttpClient.parseRequest url
    let request = baseRequest { method = "GET"
                              , secure = True
                              , requestHeaders = headers
                              }
    response <- HttpClient.httpLbs request manager
    pure $ case Aeson.decode' (HttpClient.responseBody response) of
        Nothing -> Left . UnexpectedResponse $ Just "Could not retrieve calendar details."
        Just calendar -> Right calendar
    where
        url = baseRoute ++ "/communication/v1/calendriers/operationnel"
        queryParameter :: [(ByteString.ByteString, Maybe ByteString.ByteString)]
        queryParameter = [("idutilisateurmpo", Just $ (Encoding.encodeUtf8 . Authentication.userId . Authentication.userDetails) loginDetails)]
        headers = [ ("Authorization", Encoding.encodeUtf8 . Authentication.token . Authentication.tokenDetails $ loginDetails)
                  , ("User-Agent", "monportail-cli/v0.1.0.0")
                  ]

fetchEvents :: Manager -> Authentication.LoginDetails -> Calendar -> Time.LocalTime -> Time.LocalTime -> IO (Either CalendarError [Event])
fetchEvents manager loginDetails calendar startingDate endingDate = do
    baseRequest <- HttpClient.setQueryString queryParameter <$> HttpClient.parseRequest url
    let request = baseRequest { method = "GET"
                              , secure = True
                              , requestHeaders = headers
                              }
    response <- HttpClient.httpLbs request manager
    pure $ case Aeson.decode' (HttpClient.responseBody response) of
        Nothing -> Left . UnexpectedResponse $ Just "Could not retrieve events."
        Just events' -> Right . events $ events'
    where
        url = baseRoute ++ "/communication/v1/calendriers/evenements.v2"
        queryParameter :: [(ByteString.ByteString, Maybe ByteString.ByteString)]
        queryParameter = [ ("idutilisateurmpo", (Just . Encoding.encodeUtf8 . Authentication.userId . Authentication.userDetails) loginDetails)
                         , ("idcalendrier", (Just . Encoding.encodeUtf8 . calendarId) calendar)
                         , ("horodatedebut", (Just . formatTime) startingDate)
                         , ("horodatefin", (Just . formatTime) endingDate)
                         ]
        headers = [ ("Authorization", Encoding.encodeUtf8 . Authentication.token . Authentication.tokenDetails $ loginDetails)
                  , ("User-Agent", "monportail-cli/v0.1.0.0")
                  ]

formatTime :: Time.LocalTime -> ByteString.ByteString
formatTime = Char8.pack . TimeFormat.formatTime TimeFormat.defaultTimeLocale "%FT%X.000Z"

data Calendar = Calendar { calendarId :: Text
                         , clientId :: Text
                         , calendarType :: Text
                         , name :: Text
                         , metadata :: Metadata
                         , accessLevel :: Text
                         , changeNumber :: Integer
                         }
                         deriving (Show, Eq)

instance FromJSON Calendar where
    parseJSON = Aeson.withObject "Calendar" $ \value -> Calendar
        <$> value .: "idCalendrier"
        <*> value .: "idUtilisateurMpo"
        <*> value .: "type"
        <*> value .: "nom"
        <*> value .: "metadonnees"
        <*> value .: "niveauAccesEffectif"
        <*> value .: "numeroChangement"

data Metadata = Metadata { sessionId :: Maybe Text
                         , occurenceNumber :: Maybe Text
                         , sessionCode :: Maybe Text
                         }
                         deriving (Show, Eq)

instance FromJSON Metadata where
    parseJSON = Aeson.withObject "Metadata" $ \value -> Metadata
        <$> value .:? "idSectionCours"
        <*> value .:? "numeroOccurence"
        <*> value .:? "codeSession"

newtype Events = Events { events :: [Event]
                        }
                     deriving (Show)

instance FromJSON Events where
    parseJSON = Aeson.withObject "Events" $ \value -> Events
        <$> value .: "evenements"

data Event = Event { eventCalendarId :: Text
                   , clientId :: Text
                   , sourceCalendarId :: Text
                   , communication :: Communication
                   , labels :: [Aeson.Value]
                   , startingDate :: Maybe ULavalTime
                   , endingDate :: Maybe ULavalTime
                   , allDay :: Bool
                   , object :: Text
                   , metadata :: Text
                   , accessLevel :: Text
                   , changeNumber :: Integer
                   }
                   deriving (Show)

instance FromJSON Event where
    parseJSON = Aeson.withObject "Event" $ \value -> Event
        <$> value .: "idEvenementCalendrier"
        <*> value .: "idUtilisateurMpo"
        <*> value .: "idCalendrier"
        <*> value .: "communication"
        <*> value .: "etiquettes"
        <*> value .: "horodateDebut"
        <*> value .: "horodateFin"
        <*> value .: "touteLaJournee"
        <*> value .: "objet"
        <*> value .: "metadonnees"
        <*> value .: "niveauAccesEffectif"
        <*> value .: "numeroChangement"

data Communication = Communication { code :: Text
                                   , group :: Text
                                   , category :: Text
                                   , communicationType :: Text
                                   , labels :: [Aeson.Value]
                                   , visibleToOtherUsers :: Bool
                                   , publishable :: Bool
                                   }
                                   deriving (Show)

instance FromJSON Communication where
    parseJSON = Aeson.withObject "Communication" $ \value -> Communication
        <$> value .: "codeCommunication"
        <*> value .: "codeGroupe"
        <*> value .: "codeCategorie"
        <*> value .: "typeCommunication"
        <*> value .: "etiquettes"
        <*> value .: "visibleAuxUtilisateurs"
        <*> value .: "publiable"

newtype ULavalTime = ULavalTime Time.UTCTime
    deriving (Show)

extractTime :: ULavalTime -> Time.UTCTime
extractTime (ULavalTime time) = time

toULavalTime :: Time.UTCTime -> ULavalTime
toULavalTime = ULavalTime

instance FromJSON ULavalTime where
    parseJSON = Aeson.withText "Time" $ \time -> ULavalTime
        <$> Time.parseTimeM True Time.defaultTimeLocale "%FT%X-04:00[America/New_York]" (Text.unpack time)
