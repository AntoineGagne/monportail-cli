{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Authentication ( LoginDetails (..)
                      , UserDetails (..)
                      , TokenDetails (..)
                      , Sex (..)
                      , User (..)
                      , AuthenticationError (..)
                      , fetchAuthenticationCookies
                      , fetchCredentials
                      , parseLoginDetails
                      ) where

import Data.Aeson ( FromJSON (..)
                  , ToJSON (..)
                  , (.:)
                  )
import Data.Text ( Text )
import Control.Lens ( (^.) )
import Network.HTTP.Client ( CookieJar
                           , Manager
                           , Response
                           , Request (..)
                           )
import Network.Wreq ( FormParam ( (:=) )
                    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TlsHttpClient
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as WreqSession
import qualified Network.Wreq.Types as WreqTypes

import qualified Parser


data AuthenticationError = InvalidCredentials (Maybe String)
                         | ParseError (Maybe String)
                         deriving (Show)

data LoginDetails = LoginDetails { authentified :: Bool
                                 , tokenDetails :: TokenDetails
                                 , userDetails :: UserDetails
                                 }
                                 deriving (Show, Eq)

instance FromJSON LoginDetails where
    parseJSON = Aeson.withObject "LoginDetails" $ \value -> LoginDetails
        <$> value .: "authentifie"
        <*> value .: "detailsToken"
        <*> value .: "utilisateurMpo"

data TokenDetails = TokenDetails { clientId :: Text
                                 , token :: Text
                                 , tokenType :: Text
                                 , expirationDate :: Integer
                                 , scope :: Text
                                 , userIdentifier :: Text
                                 }
                                 deriving (Show, Eq)

instance FromJSON TokenDetails where
    parseJSON = Aeson.withObject "TokenDetails" $ \value -> TokenDetails
        <$> value .: "idClient"
        <*> value .: "token"
        <*> value .: "typeToken"
        <*> value .: "dateExpiration"
        <*> value .: "scope"
        <*> value .: "identifiantUtilisateur"

data UserDetails = UserDetails { userId :: Text
                               , enaId :: Text
                               , portalUserType :: Text
                               , recordNumber :: Integer
                               , userRecordId :: Text
                               , identifier :: Text
                               , identificationNumber :: Text
                               , pid :: Text
                               , firstName :: Text
                               , lastName :: Text
                               , username :: Text
                               , emailAddress :: Text
                               , sex :: Sex
                               , favoriteLanguage :: Text
                               , isColourBlind :: Bool
                               , isSuspended :: Bool
                               , accesses :: [Text]
                               , modificationNumber :: Integer
                               }
                               deriving (Show, Eq)

instance FromJSON UserDetails where
    parseJSON = Aeson.withObject "UserDetails" $ \value -> UserDetails
        <$> value .: "idUtilisateurMpo"
        <*> value .: "idUtilisateurEna"
        <*> value .: "typeUtilisateurMpo"
        <*> value .: "numeroDossier"
        <*> value .: "idDossierIndividuEtudes"
        <*> value .: "identifiant"
        <*> value .: "nie"
        <*> value .: "pidm"
        <*> value .: "prenom"
        <*> value .: "nom"
        <*> value .: "pseudonyme"
        <*> value .: "courrielPrincipal"
        <*> value .: "sexe"
        <*> value .: "languePreferee"
        <*> value .: "daltonien"
        <*> value .: "suspendu"
        <*> value .: "acces"
        <*> value .: "numeroChangement"

data Sex = Male
         | Female
         | Other Text
         deriving (Show, Eq)

instance FromJSON Sex where
    parseJSON = Aeson.withText "Sex" $ \value -> pure . matchSex $ value
        where
            matchSex "MASCULIN" = Male
            matchSex "FEMININ"  = Female
            matchSex sex        = Other sex

data User = User { loginName :: ByteString.ByteString
                 , password :: ByteString.ByteString
                 }
                 deriving (Show)

fetchAuthenticationCookies :: Manager -> IO CookieJar
fetchAuthenticationCookies manager = do
    baseRequest <- HttpClient.setQueryString [queryParameter] <$> HttpClient.parseRequest cookieSetterBaseUrl
    let request = baseRequest { method = "GET"
                              , secure = True
                              , cookieJar = Just cookies
                              }
    response <- HttpClient.responseOpenHistory request manager
    currentTime <- Clock.getCurrentTime
    let updatedCookies = updateCookies currentTime cookies (HttpClient.hrRedirects response)
    pure $ updateCookies currentTime updatedCookies [(HttpClient.hrFinalRequest response, HttpClient.hrFinalResponse response)]
    where
        cookieSetterBaseUrl = "https://monportail.ulaval.ca/auth/deleguer/"
        queryParameter = ("urlretour", Just "https://monportail.ulaval.ca")
        cookies = HttpClient.createCookieJar []

updateCookies :: Clock.UTCTime -> CookieJar -> [(Request, Response a)] -> CookieJar
updateCookies currentTime = foldr updateCookies'
    where
        updateCookies' (request, response) cookies'  =
            let (updatedCookies, _) = HttpClient.updateCookieJar response request currentTime cookies'
                in updatedCookies

fetchCredentials :: User -> CookieJar -> IO (Wreq.Response LazyByteString.ByteString)
fetchCredentials user cookies =
    WreqSession.withSessionControl (Just cookies) TlsHttpClient.tlsManagerSettings $ \session ->
        WreqSession.post session baseUrl queryParameters
    where
            baseUrl = "https://authentification.ulaval.ca/my.policy"
            queryParameters = [ "rememberMe" := ("0" :: Text)
                              , "username" := loginName user
                              , "password" := password user
                              , "vhost" := ("standard" :: Text)
                              ]

parseLoginDetails :: LazyByteString.ByteString -> Either AuthenticationError LoginDetails
parseLoginDetails response = 
    case Parser.parseLoginDetails response of
        Left error' -> Left $ InvalidCredentials (Just . show $ error')
        Right details -> decodeLoginDetails details
    where
        decodeLoginDetails :: String -> Either AuthenticationError Authentication.LoginDetails
        decodeLoginDetails responseBody = 
            case Aeson.decode . Char8.pack . filter (/= '\\') $ responseBody of
                Nothing -> Left $ ParseError (Just "Could not decode the login details.")
                Just details -> Right details
