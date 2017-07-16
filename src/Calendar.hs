{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Calendar ( Calendar (..)
                , Metadata (..)
                , Event (..)
                , Events (..)
                , Communication (..)
                , extractTime
                , toULavalTime
                ) where

import Data.Aeson ( FromJSON (..)
                  , ToJSON (..)
                  , (.:)
                  , (.:?)
                  )
import Data.Text ( Text )

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time


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
                   , calendarId :: Text
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
