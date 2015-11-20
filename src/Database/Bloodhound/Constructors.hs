{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}

module Database.Bloodhound.Constructors where

import           Data.Default.Generics
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Database.Bloodhound.Types

class IsQuery a where
    mkQuery :: a -> Query

instance IsQuery MoreLikeThisQuery where
    mkQuery = QueryMoreLikeThisQuery

class MoreLikeThis a where
    moreLikeThis :: a -> MoreLikeThisQuery

instance MoreLikeThis Text where
    moreLikeThis t = def { moreLikeThisQuery = Right t }

instance MoreLikeThis [Doc] where
    moreLikeThis d = def { moreLikeThisQuery = Left d }

instance MoreLikeThis [DocId] where
    moreLikeThis d = def { moreLikeThisQuery = Left $ map mkDoc d }

class IsAggregation a where
    mkAggregation :: a -> Aggregation

instance IsAggregation Aggregation where
    mkAggregation = id

instance IsAggregation FilterAggregation where
    mkAggregation = FilterAgg

instance IsAggregation SignificantTermsAggregation where
    mkAggregation = SigTermsAgg

mkAggregations' :: IsAggregation a => Text -> a -> Aggregations
mkAggregations' t a = mkAggregations t (mkAggregation a)

fieldNames :: Text -> [FieldName]
fieldNames = map FieldName . T.words

mkSignificantTermsAggregation :: FieldName -> SignificantTermsAggregation
mkSignificantTermsAggregation f = def { sigField = f }

instance Default Size where
    def = Size 10

instance Default SearchType where
    def = SearchTypeQueryAndFetch

instance Default Search
instance Default MoreLikeThisQuery
instance Default SignificantTermsAggregation
instance Default FieldName
instance Default From
instance Default SignificanceScore
instance Default Script
