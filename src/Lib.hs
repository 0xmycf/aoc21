{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT)

import qualified Text.Parsec as Parsec

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"
