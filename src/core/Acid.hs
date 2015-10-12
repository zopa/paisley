{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RankNTypes, TypeFamilies,
             ViewPatterns, TypeSynonymInstances #-}

module Acid where

import Control.Lens 
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Acid.Advanced
import Data.Data (Data,Typeable)
import Data.Map.Strict (Map, maxViewWithKey)
import qualified Data.Map.Strict as Map
import Data.Generics
import Data.SafeCopy

import Types

data CSA = CSA { _members :: Map Int Membership}

csa :: Iso (Map Int Membership) (Map Int Membership) CSA CSA 
csa = iso CSA _members 

insertAsMax :: v -> Map Int v -> Map Int v
insertAsMax v   (maxViewWithKey -> Nothing)        = Map.singleton 1 v
insertAsMax v m@(maxViewWithKey -> Just ((k,_),_)) = Map.insert (succ k) v m

newMembership :: Membership -> Update CSA ()
newMembership = modify . under csa . insertAsMax

$(deriveSafeCopy 0 'base ''CSA)

$(makeAcidic ''CSA [ 'newMembership
                       ])
