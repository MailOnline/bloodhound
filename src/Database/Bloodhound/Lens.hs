{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Bloodhound.Lens
    ( module Database.Bloodhound.Lens.Types
    , module Database.Bloodhound.Types
    , module Database.Bloodhound.Constructors
    ) where

import           Database.Bloodhound.Constructors
import           Database.Bloodhound.Lens.Types
import           Database.Bloodhound.Types        hiding (Search (..),
                                                   SearchResult (..), aggs)
import           Database.Bloodhound.Types        (Search, SearchResult)
