module Database.Bloodhound.Lens.Util where

import           Control.Lens
import           Data.Char
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH

dropPrefixNamer :: String -> Name -> [Name] -> Name -> [DefName]
dropPrefixNamer pref _ fields field = maybeToList $ do
    fieldPart <- stripMaxLc (nameBase field)
    method    <- computeMethod fieldPart
    let cls = "Has" ++ fieldPart
    return (MethodName (mkName cls) (mkName method))
  where
    stripMaxLc f = do x <- stripPrefix optUnderscore f
                      stripPrefix pref x
    optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]
    computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
    computeMethod _                  = Nothing

dropPrefixFields :: String -> LensRules
dropPrefixFields x = abbreviatedFields & lensField .~ dropPrefixNamer x

sameNamer :: Name -> [Name] -> Name -> [DefName]
sameNamer _ _ field = maybeToList $ do
    let fieldName = nameBase field
    fieldClass <- computeClass (nameBase field)
    let cls = "Has" ++ fieldClass
    return (MethodName (mkName cls) (mkName fieldName))
  where
    computeClass (x:xs) | isLower x = Just (toUpper x : xs)
    computeClass _                  = Nothing

sameNameFields :: LensRules
sameNameFields = abbreviatedFields & lensField .~ sameNamer
