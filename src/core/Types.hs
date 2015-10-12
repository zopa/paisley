{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, DeriveGeneric, DefaultSignatures, StandaloneDeriving #-}
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

data WinterMenu = Winter { _winter :: Int }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data FallMenu   = Fall { _vegetable :: Int
                       , _egg       :: Int
                       , _fruit     :: Int
                       } 
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Season = Fall2015   FallMenu
            | Winter2016 WinterMenu
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Membership = Membership { _shareholder :: String
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

$(deriveSafeCopy 0 'base ''Description)
$(deriveSafeCopy 0 'base ''CardCheckResult)
$(deriveSafeCopy 0 'base ''CardChecks)
$(deriveSafeCopy 0 'base ''Currency)
$(deriveSafeCopy 0 'base ''Amount)
$(deriveSafeCopy 0 'base ''Card)
$(deriveSafeCopy 0 'base ''Charge)
$(deriveSafeCopy 0 'base ''ChargeId)
$(deriveSafeCopy 0 'base ''FallMenu)
$(deriveSafeCopy 0 'base ''WinterMenu)
$(deriveSafeCopy 0 'base ''Season)
$(deriveSafeCopy 0 'base ''Membership)
