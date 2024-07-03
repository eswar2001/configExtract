{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances,DeriveDataTypeable,DeriveAnyClass #-}

module ConfigExtract.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text
import Data.Data
import qualified Data.Map as Map

data CoreFunction = CoreFunction {
    vars :: [Text]
    , lits :: [Text]
    , types :: [Text]
    , caseRelationsNested :: [CaseCollate]
    , coreName :: Text
}
    deriving (Show,Generic,ToJSON,FromJSON)

type FName = Text
type FRelation = Text
type FScrut = Text
type FValue = Text
type FType = Text

data Node = 
        NFunction FName [Node]
        | NCase (FScrut,[Node]) [(FRelation,[Node],[Node])] 
        | NType [Text]
        | NConstant FValue FType Bool
        | DFunction FName Text
        | CalledFunction FName Text Text
        | NLambda FName [Node]
    deriving (Show,Generic,ToJSON,FromJSON)

data CaseCollate = CaseCollate Text [(Text,[CaseCollate])]
    deriving (Show,Generic,ToJSON,FromJSON)

-- instance ToJSON CaseCollate where
--     toJSON (CaseCollate condition (x:xs)) = object ["condition" .= (replace "\n" "" condition), "relations" .= map (\(r,a) -> object ["relation" .= toJSON r , "next" .= toJSON a]) (x:xs)]
--     toJSON (CaseCollate condition [x]) = object ["condition" .= (replace "\n" "" condition), "relations" .= map (\(r,a) -> object ["relation" .= toJSON r , "next" .= toJSON a]) [x]]
--     toJSON (CaseCollate condition []) =object ["condition" .= (replace "\n" "" condition)]
       