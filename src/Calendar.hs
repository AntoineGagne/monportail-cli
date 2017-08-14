{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Calendar ( Calendar (..)
                , Event (..)
                , Events (..)
                , Communication (..)
                , ULavalTime
                , fromULavalTime
                , toULavalTime
                , fetchCalendarDetails
                , fetchCurrentCalendars
                , fetchEvents
                , sortEvents
                , eventDate
                ) where

import Control.Monad.Except ( ExceptT (..)
                            , throwError
                            , liftIO
                            )
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
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
import qualified Exceptions


baseRoute :: String
baseRoute = "https://monportail.ulaval.ca"

fetchCalendarDetails
    :: Manager
    -> Authentication.LoginDetails
    -> ExceptT Exceptions.MonPortailException IO Calendar
fetchCalendarDetails manager loginDetails = either throwError pure =<< liftIO (fetchCalendarDetails' manager loginDetails)

fetchCalendarDetails'
    :: Manager
    -> Authentication.LoginDetails
    -> IO (Either Exceptions.MonPortailException Calendar)
fetchCalendarDetails' manager loginDetails = do
    request <- createBaseRequest loginDetails queryParameter url
    response <- HttpClient.httpLbs request manager
    pure $ case Aeson.eitherDecode' (HttpClient.responseBody response) of
        Left message -> Left . Exceptions.throwUnexpectedResponseError $ Just ("Could not retrieve calendar details. Failed with following errors: " ++ message)
        Right calendar -> Right calendar
    where
        url = baseRoute ++ "/communication/v1/calendriers/operationnel"
        queryParameter :: [(ByteString.ByteString, Maybe ByteString.ByteString)]
        queryParameter = [("idutilisateurmpo", Just $ (Encoding.encodeUtf8 . Authentication.userId . Authentication.userDetails) loginDetails)]

fetchEvents
    :: Manager
    -> Authentication.LoginDetails
    -> Calendar
    -> Time.LocalTime
    -> Time.LocalTime
    -> ExceptT Exceptions.MonPortailException IO [Event]
fetchEvents manager loginDetails calendar startingDate endingDate = either throwError pure =<< liftIO (fetchEvents' manager loginDetails calendar startingDate endingDate)

fetchEvents' 
    :: Manager
    -> Authentication.LoginDetails
    -> Calendar
    -> Time.LocalTime
    -> Time.LocalTime
    -> IO (Either Exceptions.MonPortailException [Event])
fetchEvents' manager loginDetails calendar startingDate endingDate = do
    request <- createBaseRequest loginDetails queryParameter url
    response <- HttpClient.httpLbs request manager
    pure $ case Aeson.eitherDecode' (HttpClient.responseBody response) of
        Left message -> Left . Exceptions.throwUnexpectedResponseError $ Just ("Could not retrieve events. Failed with following errors: " ++ message)
        Right events' -> Right . events $ events'
    where
        url = baseRoute ++ "/communication/v1/calendriers/evenements.v2"
        queryParameter :: [(ByteString.ByteString, Maybe ByteString.ByteString)]
        queryParameter = [ ("idutilisateurmpo", (Just . Encoding.encodeUtf8 . Authentication.userId . Authentication.userDetails) loginDetails)
                         , ("idcalendrier", (Just . Encoding.encodeUtf8 . calendarId) calendar)
                         , ("horodatedebut", (Just . formatTime) startingDate)
                         , ("horodatefin", (Just . formatTime) endingDate)
                         ]

createBaseRequest
    :: Authentication.LoginDetails
    -> [(ByteString.ByteString, Maybe ByteString.ByteString)]
    -> String
    -> IO Request
createBaseRequest loginDetails queryParameters url = do
    baseRequest <- HttpClient.setQueryString queryParameters <$> HttpClient.parseRequest url
    pure $ baseRequest { method = "GET"
                       , secure = True
                       , requestHeaders = headers
                       }
    where
        headers = [ ("Authorization", Encoding.encodeUtf8 . Authentication.token . Authentication.tokenDetails $ loginDetails)
                  , ("User-Agent", "monportail-cli/v0.1.0")
                  ]

fetchCurrentCalendars
    :: Manager
    -> Authentication.LoginDetails
    -> ExceptT Exceptions.MonPortailException IO [Calendar]
fetchCurrentCalendars manager loginDetails =
    either throwError pure =<< liftIO (fetchCurrentCalendars' manager loginDetails )

fetchCurrentCalendars'
    :: Manager
    -> Authentication.LoginDetails
    -> IO (Either Exceptions.MonPortailException [Calendar])
fetchCurrentCalendars' manager loginDetails = do
    request <- createBaseRequest loginDetails [] url
    response <- HttpClient.httpLbs request manager
    pure $ case Aeson.eitherDecode' (HttpClient.responseBody response) of
        Left message -> Left . Exceptions.throwUnexpectedResponseError $ Just ("Could not retrieve current calendars. Failed with following errors: " ++ message)
        Right currentCalendar -> Right . concatenateCalendars $ currentCalendar
    where
        url = baseRoute ++ "/communication/v1/calendriers/courants/" ++ userId'
        userId' = Text.unpack . Authentication.userId . Authentication.userDetails $ loginDetails
        concatenateCalendars currentCalendar' = generalCalendars currentCalendar' ++ concatMap calendars (sessionCalendars currentCalendar')

formatTime :: Time.LocalTime -> ByteString.ByteString
formatTime = Char8.pack . TimeFormat.formatTime TimeFormat.defaultTimeLocale "%FT%X.000Z"

data CurrentCalendar = CurrentCalendar { clientId :: Text
                                       , generalCalendars :: [Calendar]
                                       , sessionCalendars :: [SessionCalendar]
                                       } deriving (Show, Eq)

instance FromJSON CurrentCalendar where
    parseJSON = Aeson.withObject "CurrentCalendar" $ \value -> CurrentCalendar
        <$> value .: "idUtilisateurMpo"
        <*> value .: "calendriersGeneraux"
        <*> value .: "calendriersSession"

data SessionCalendar = SessionCalendar { sessionCode :: Text
                                       , season :: Text
                                       , civilYear :: Integer
                                       , currentSession :: Bool
                                       , nextSession :: Bool
                                       , calendars :: [Calendar]
                                       } deriving (Show, Eq)

instance FromJSON SessionCalendar where
    parseJSON = Aeson.withObject "SessionCalendar" $ \value -> SessionCalendar
        <$> value .: "codeSession"
        <*> value .: "saisonUniversitaire"
        <*> value .: "anneeCivile"
        <*> value .: "sessionCourante"
        <*> value .: "sessionSuivante"
        <*> value .: "calendriers"

data Calendar = Calendar { calendarId :: Text
                         , clientId :: Text
                         , calendarType :: Text
                         , name :: Text
                         , metadata :: Aeson.Value
                         , accessLevel :: Text
                         , changeNumber :: Maybe Integer
                         , informationCorrelationSource :: Maybe Aeson.Value
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
        <*> value .:? "numeroChangement"
        <*> value .:? "infoCorrelationSource"

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
                   , correlationSource :: Maybe Aeson.Value
                   , labels :: [Aeson.Value]
                   , startingDate :: Maybe ULavalTime
                   , endingDate :: Maybe ULavalTime
                   , allDay :: Bool
                   , object :: Text
                   , metadata :: Aeson.Value
                   , accessLevel :: Text
                   , changeNumber :: Integer
                   }
                   deriving (Eq, Show)

instance FromJSON Event where
    parseJSON = Aeson.withObject "Event" $ \value -> Event
        <$> value .: "idEvenementCalendrier"
        <*> value .: "idUtilisateurMpo"
        <*> value .: "idCalendrier"
        <*> value .: "communication"
        <*> value .:? "infoCorrelationSource"
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
                                   deriving (Eq, Show)

instance FromJSON Communication where
    parseJSON = Aeson.withObject "Communication" $ \value -> Communication
        <$> value .: "codeCommunication"
        <*> value .: "codeGroupe"
        <*> value .: "codeCategorie"
        <*> value .: "typeCommunication"
        <*> value .: "etiquettes"
        <*> value .: "visibleAuxUtilisateurs"
        <*> value .: "publiable"

newtype ULavalTime = ULavalTime Time.LocalTime
    deriving (Eq, Ord, Show)

fromULavalTime :: ULavalTime -> Time.LocalTime
fromULavalTime (ULavalTime time) = time

toULavalTime :: Time.LocalTime -> ULavalTime
toULavalTime = ULavalTime

instance FromJSON ULavalTime where
    parseJSON = Aeson.withText "Time" $ \time -> ULavalTime
        <$> Time.parseTimeM True Time.defaultTimeLocale "%FT%X%Z" (take 19 . Text.unpack $ time)

sortEvents :: [Calendar.Event] -> (Map.Map Time.Day [Calendar.Event], [Calendar.Event])
sortEvents events = (eventsByStartingDate, eventsLongerThanOneDay)
    where eventsByStartingDate = List.foldl' (\m event -> Map.insertWith (++) (eventStartingDate event) (pure event) m) Map.empty filteredEvents
          filteredEvents = List.sortOn Calendar.startingDate $ filter (compareEventDates (==)) events
          eventsLongerThanOneDay = filter (compareEventDates (/=)) events
          compareEventDates compare event = compare (eventStartingDate event) (eventEndingDate event)
          eventStartingDate = eventDate Calendar.startingDate
          eventEndingDate = eventDate Calendar.endingDate

eventDate :: (Calendar.Event -> Maybe Calendar.ULavalTime) -> Calendar.Event -> Time.Day
eventDate accessor event = Maybe.fromJust $ Time.localDay . Calendar.fromULavalTime <$> accessor event
