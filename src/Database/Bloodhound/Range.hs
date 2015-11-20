{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Database.Bloodhound.Range
    ( IsRange(..), LT(..), LTE(..), GT(..), GTE(..)
    ) where

import           Data.Time

import           Database.Bloodhound.Types

class IsRange a where
    range :: a -> RangeValue

instance IsRange RangeValue where
    range = id

instance IsRange LessThanEqD where
    range = RangeDateLte

instance IsRange LessThanD where
    range = RangeDateLt

instance IsRange GreaterThanEqD where
    range = RangeDateGte

instance IsRange GreaterThanD where
    range = RangeDateGt

instance IsRange LessThanEq where
    range = RangeDoubleLte

instance IsRange LessThan where
    range = RangeDoubleLt

instance IsRange GreaterThanEq where
    range = RangeDoubleGte

instance IsRange GreaterThan where
    range = RangeDoubleGt


class IsRange r => LT l u r | l u -> r where
    (<.) :: l -> u -> r

instance LT () Double LessThan where
    () <. d = LessThan d

instance LT Double () GreaterThan where
    d <. () = GreaterThan d

instance LT GreaterThan Double RangeValue where
    g <. d = RangeDoubleGtLt g (LessThan d)

instance LT GreaterThanEq Double RangeValue where
    g <. d = RangeDoubleGteLt g (LessThan d)

instance LT Double LessThan RangeValue where
    d <. g = RangeDoubleGtLt (GreaterThan d) g

instance LT Double LessThanEq RangeValue where
    d <. g = RangeDoubleGtLte (GreaterThan d) g

instance LT () UTCTime LessThanD where
    () <. d = LessThanD d

instance LT UTCTime () GreaterThanD where
    d <. () = GreaterThanD d

instance LT GreaterThanD UTCTime RangeValue where
    g <. d = RangeDateGtLt g (LessThanD d)

instance LT GreaterThanEqD UTCTime RangeValue where
    g <. d = RangeDateGteLt g (LessThanD d)

instance LT UTCTime LessThanD RangeValue where
    d <. g = RangeDateGtLt (GreaterThanD d) g

instance LT UTCTime LessThanEqD RangeValue where
    d <. g = RangeDateGtLte (GreaterThanD d) g


class IsRange r => LTE l u r | l u -> r where
    (<=.) :: l -> u -> r

instance LTE () Double LessThanEq where
    () <=. d = LessThanEq d

instance LTE Double () GreaterThanEq where
    d <=. () = GreaterThanEq d

instance LTE GreaterThanEq Double RangeValue where
    g <=. d = RangeDoubleGteLte g (LessThanEq d)

instance LTE GreaterThan Double RangeValue where
    g <=. d = RangeDoubleGtLte g (LessThanEq d)

instance LTE Double LessThanEq RangeValue where
    d <=. g = RangeDoubleGteLte (GreaterThanEq d) g

instance LTE Double LessThan RangeValue where
    d <=. g = RangeDoubleGteLt (GreaterThanEq d) g

instance LTE () UTCTime LessThanEqD where
    () <=. d = LessThanEqD d

instance LTE UTCTime () GreaterThanEqD where
    d <=. () = GreaterThanEqD d

instance LTE GreaterThanEqD UTCTime RangeValue where
    g <=. d = RangeDateGteLte g (LessThanEqD d)

instance LTE GreaterThanD UTCTime RangeValue where
    g <=. d = RangeDateGtLte g (LessThanEqD d)

instance LTE UTCTime LessThanEqD RangeValue where
    d <=. g = RangeDateGteLte (GreaterThanEqD d) g

instance LTE UTCTime LessThanD RangeValue where
    d <=. g = RangeDateGteLt (GreaterThanEqD d) g


class LT l u r => GT u l r | u l -> r where
    (>.) :: u -> l -> r
    x >. y = y <. x

class LTE l u r => GTE u l r | u l -> r where
    (>=.) :: u -> l -> r
    x >=. y = y <=. x
