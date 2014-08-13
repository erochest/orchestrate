{-# LANGUAGE OverloadedStrings #-}


module Orchestrate.Spec.Types where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                  as T

import           Database.Orchestrate.Types


data Person = Person
            { name :: T.Text
            , age  :: Int
            } deriving (Eq, Show)

instance FromJSON Person where
    parseJSON (Object o) =   Person
                         <$> o .: "name"
                         <*> o .: "age"
    parseJSON _          = mzero

instance ToJSON Person where
    toJSON (Person n a) = object [ "name" .= n
                                 , "age"  .= a
                                 ]

instance OrchestrateData Person where
    tableName _          = "test-coll"
    dataKey (Person n _) = n

personName :: Functor f => (T.Text -> f T.Text) -> Person -> f Person
personName f (Person n a) = fmap (`Person` a) (f n)

personAge :: Functor f => (Int -> f Int) -> Person -> f Person
personAge f (Person n a) = fmap (Person n) (f a)
