module Exceptions
    ( MonPortailException (..)
    , throwInvalidCredentialsError
    , throwUnexpectedResponseError
    ) where


data MonPortailException = CalendarError CalendarError
                         | ParseError (Maybe String)
                         | AuthenticationError AuthenticationError
                         deriving (Show)

throwInvalidCredentialsError :: Maybe String -> MonPortailException
throwInvalidCredentialsError = AuthenticationError . InvalidCredentials

throwUnexpectedResponseError :: Maybe String -> MonPortailException
throwUnexpectedResponseError = CalendarError . UnexpectedResponse

newtype AuthenticationError = InvalidCredentials (Maybe String)
                         deriving (Show)

newtype CalendarError = UnexpectedResponse (Maybe String)
    deriving (Show)
