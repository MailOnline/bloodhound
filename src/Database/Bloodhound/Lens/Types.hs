{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Database.Bloodhound.Lens.Types where

import           Control.Lens

import           Database.Bloodhound.Lens.Util
import           Database.Bloodhound.Types

makeLensesWith (dropPrefixFields "moreLikeThis") ''MoreLikeThisQuery
makeLensesWith (dropPrefixFields "sig") ''SignificantTermsAggregation
makeLensesWith sameNameFields ''Search
makeLensesWith sameNameFields ''SearchResult
