{-# LANGUAGE OverloadedStrings #-}

module Parser ( parseLoginDetails
              ) where

import Text.Parsec ( try )
import Text.Parsec.ByteString.Lazy ( Parser )
import Text.Parsec.Char ( anyChar
                        , char
                        , endOfLine
                        , spaces
                        , string
                        )
import Text.Parsec.Combinator ( between
                              , many1
                              , manyTill
                              , skipMany1
                              )
import Text.Parsec.Error ( ParseError )
import Text.Parsec.Prim ( parse )
import Text.Parsec.Token ( GenTokenParser (..)
                         )

import Data.ByteString.Lazy ( ByteString )


parseLoginDetails :: ByteString -> Either ParseError String
parseLoginDetails = parse loginDetailsToken "Response"

loginDetailsToken :: Parser String
loginDetailsToken = do
    manyTill anyChar $ try (string "serviceSecureStorage.setItem(\"mpo.securite.contexteUtilisateur\", \"")
    manyTill anyChar $ try (string "\");")
