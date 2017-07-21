{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module TestAuthentication
    ( tests
    ) where

import Test.HUnit ( (@=?)
                  , Test (..)
                  )
import Data.ByteString.Lazy ( ByteString (..) )

import Authentication ( UserDetails (..)
                      , LoginDetails (..)
                      , TokenDetails (..)
                      , Sex (..)
                      )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Maybe as Maybe

import qualified Authentication as Authentication


loginDetailsJsonString :: ByteString
loginDetailsJsonString = ByteString.concat ["{"
                                           , " \"authentifie\": true, "
                                           , " \"detailsToken\":  "
                                           , tokenDetailsJsonString
                                           , ", \"utilisateurMpo\": "
                                           , userDetailsJsonString
                                           , " }"
                                           ]

loginDetails :: LoginDetails
loginDetails = LoginDetails { authentified = True
                            , tokenDetails = TestAuthentication.tokenDetails
                            , userDetails = TestAuthentication.userDetails
                            }

tokenDetailsJsonString = "{ \
                          \ \"idClient\": \"l7xxd848c2669ccc41b29d0dd13e034u23af\", \
                          \ \"token\": \"40eb4e1a-c9a5-4a75-a7d6-282954df2e4e\", \
                          \ \"typeToken\": \"Bearer\", \
                          \ \"dateExpiration\": 1498929746282, \
                          \ \"scope\": \"scope_test\", \
                          \ \"identifiantUtilisateur\": \"josmit317\" \
                        \ }"

tokenDetails :: TokenDetails
tokenDetails = TokenDetails { clientId = "l7xxd848c2669ccc41b29d0dd13e034u23af"
                            , token = "40eb4e1a-c9a5-4a75-a7d6-282954df2e4e"
                            , tokenType = "Bearer"
                            , expirationDate = 1498929746282
                            , scope = "scope_test"
                            , userIdentifier = "josmit317"
                            }

userDetailsJsonString = "{ \
                         \ \"idUtilisateurMpo\": \"6036900\", \
                         \ \"idUtilisateurEna\": \"6036900\", \
                         \ \"typeUtilisateurMpo\": \"institutionnel\", \
                         \ \"numeroDossier\": 6036900, \
                         \ \"idDossierIndividuEtudes\": \"6036900\", \
                         \ \"identifiant\": \"josmit317\", \
                         \ \"nie\": \"111675273\", \
                         \ \"pidm\": \"6036900\", \
                         \ \"prenom\": \"John\", \
                         \ \"nom\": \"Smith\", \
                         \ \"pseudonyme\": \"John Smith\", \
                         \ \"courrielPrincipal\": \"john.smith.2@ulaval.ca\", \
                         \ \"sexe\": \"MASCULIN\", \
                         \ \"languePreferee\": \"FR\", \
                         \ \"daltonien\": false, \
                         \ \"suspendu\": false, \
                         \ \"acces\": [], \
                         \ \"numeroChangement\": 4191208 \
                       \ }"

userDetails :: UserDetails
userDetails = UserDetails { userId = "6036900"
                          , enaId = "6036900"
                          , portalUserType = "institutionnel"
                          , recordNumber = 6036900
                          , userRecordId = "6036900"
                          , identifier = "josmit317"
                          , identificationNumber = "111675273"
                          , pid = "6036900"
                          , firstName = "John"
                          , lastName = "Smith"
                          , username = "John Smith"
                          , emailAddress = "john.smith.2@ulaval.ca"
                          , sex = Male
                          , favoriteLanguage = "FR"
                          , isColourBlind = False
                          , isSuspended = False
                          , accesses = []
                          , modificationNumber = 4191208
                          }

tests = TestList [ givenAValidUserDetailsJsonString_whenParsingUserDetails_thenReturnCorrectUserDetails
                 , givenAValidTokenDetailsJsonString_whenParsingTokenDetails_thenReturnCorrectTokenDetails
                 , givenAValidLoginDetailsJsonString_whenParsingLoginDetails_thenReturnCorrectLoginDetails
                 ]

givenAValidUserDetailsJsonString_whenParsingUserDetails_thenReturnCorrectUserDetails =
    TestCase (decodedValue @=? TestAuthentication.userDetails)
        where 
            decodedValue :: UserDetails
            decodedValue = Maybe.fromJust . Aeson.decode $ userDetailsJsonString

givenAValidTokenDetailsJsonString_whenParsingTokenDetails_thenReturnCorrectTokenDetails =
    TestCase (decodedValue @=? TestAuthentication.tokenDetails)
        where
            decodedValue :: TokenDetails
            decodedValue = Maybe.fromJust . Aeson.decode $ tokenDetailsJsonString

givenAValidLoginDetailsJsonString_whenParsingLoginDetails_thenReturnCorrectLoginDetails =
    TestCase (decodedValue @=? loginDetails)
        where
            decodedValue :: LoginDetails
            decodedValue = Maybe.fromJust . Aeson.decode $ loginDetailsJsonString

givenAValidSexJsonString_whenParsingSex_thenReturnCorrectSex =
    TestCase (decodedValue @=? Male)
        where
            decodedValue :: Sex
            decodedValue = Maybe.fromJust . Aeson.decode $ "MASCULIN"
