{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric, DefaultSignatures, StandaloneDeriving, TypeFamilies #-}
module Types where

import Control.Lens
import Data.Data (Data,Typeable)
import Data.Serialize
import Data.Time.Calendar
import Data.Time.Clock (DiffTime)
import Data.Serialize.Text ()
import GHC.Generics
import Data.SafeCopy
import Web.Stripe.Card
import Web.Stripe.Charge

data WinterMenu = Winter { _winter :: Int
                         , _wegg   :: Int
                         }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- Counting by half-shares
data FallMenu   = Fall { _vegetable :: Int
                       , _egg       :: Int
                       , _fruit     :: Int
                       }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Season = Fall2015   FallMenu
            | Winter2016 WinterMenu
            | Summer2016 FallMenu
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Membership = Membership { _shareholder :: String
                             , _phone       :: String
                             , _email       :: String
                             , _alternates  :: [String]
                             , _season      :: Season
                             , _cost        :: Int
                             , _payment     :: Maybe Charge
                             }
    deriving (Show, Data, Typeable, Generic)

deriving instance Data Amount
deriving instance Data Card
deriving instance Data CardChecks
deriving instance Data CardCheckResult
deriving instance Data Charge
deriving instance Data ChargeId
deriving instance Data Currency
deriving instance Data Description

deriving instance Generic Amount
deriving instance Generic Card
deriving instance Generic CardChecks
deriving instance Generic CardCheckResult
deriving instance Generic Charge
deriving instance Generic ChargeId
deriving instance Generic Currency
deriving instance Generic Description
deriving instance Generic Day

deriving instance Generic UTCTime

data ShareType = Vegetable
               | Egg
               | Fruit
   deriving (Eq, Ord, Show)

shareCount :: ShareType -> Season -> Maybe Int
shareCount v (Fall2015 menu) = case v of
  Vegetable -> Just $ _vegetable menu
  Egg       -> Just $ _egg       menu
  Fruit     -> Just $ _egg       menu
shareCount v (Winter2016 menu) = case v of
  Vegetable -> Just $ _winter menu
  _         -> Nothing

instance Serialize Amount
instance Serialize Day
instance Serialize Card
instance Serialize CardChecks
instance Serialize CardCheckResult
instance Serialize Charge
instance Serialize ChargeId
instance Serialize Currency
instance Serialize Description
instance Serialize WinterMenu
instance Serialize FallMenu
instance Serialize Season
instance Serialize Membership

instance Serialize UTCTime where
    get = UTCTime <$> get <*> get
    put (UTCTime day time) = put day >> put time

instance Serialize DiffTime where
    get = fromRational <$> get
    put = put . toRational
------------------------------------------------------------
-- Old versions
------------------------------------------------------------


data WinterMenu_v0 = Winter_v0 { _winter_v0 :: Int }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''WinterMenu_v0)

data Season_v0 = Fall2015_v0  FallMenu
               | Winter2016_v0 WinterMenu_v0
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''Season_v0)

data Season_v1 = Fall2015_v1   FallMenu
               | Winter2016_v1 WinterMenu
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Migrate WinterMenu where
  type MigrateFrom (WinterMenu) = WinterMenu_v0
  migrate (Winter_v0 i) = Winter i 0

instance Migrate Season_v1 where
  type MigrateFrom (Season_v1) = Season_v0
  migrate (Fall2015_v0 f)   = Fall2015_v1 f
  migrate (Winter2016_v0 w) = Winter2016_v1 (migrate w)

$(deriveSafeCopy 1 'extension ''Season_v1)

instance Migrate Season where
  type MigrateFrom (Season) = Season_v1
  migrate (Fall2015_v1 f)   = Fall2015 f
  migrate (Winter2016_v1 w) = Winter2016 w

$(deriveSafeCopy 0 'base ''Description)
$(deriveSafeCopy 0 'base ''CardCheckResult)
$(deriveSafeCopy 0 'base ''CardChecks)
$(deriveSafeCopy 0 'base ''Currency)
$(deriveSafeCopy 0 'base ''Amount)
$(deriveSafeCopy 0 'base ''Card)
$(deriveSafeCopy 0 'base ''Charge)
$(deriveSafeCopy 0 'base ''ChargeId)
$(deriveSafeCopy 0 'base ''FallMenu)
$(deriveSafeCopy 1 'extension ''WinterMenu)
$(deriveSafeCopy 2 'extension ''Season)
$(deriveSafeCopy 0 'base ''Membership)
