{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections,
             TypeFamilies, FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, ViewPatterns  #-}
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad (void)
import           Control.Monad.Reader
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import           Data.Monoid
import qualified Data.Map.Strict as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import           Data.Serialize
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Debug.Trace
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Text.Printf (printf)
import           Web.Stripe.Charge
import           Web.Stripe.Client (SecretKey(..), StripeVersion(..))
import           Web.Stripe.Token
import           Acid
import           Types

main :: IO ()
main = bracket
  (openLocalState (CSA mempty))
  createCheckpointAndClose
  (quickHttpServe . runReaderT site )

site :: AcidSnap CSA ()
site =
    route [ ("charge",            charge )
  --        , ("identifications",   handleIdentifications)
  --        , ("relationships",     handleRelationships)
  --        , ("resources",         serveDirectory "resources")
          ] <|>
    serveDirectory "webform.jsexe"

type AcidSnap st = ReaderT (AcidState st) Snap

failWith :: MonadSnap m => Int -> String -> m ()
failWith code msg = do
  modifyResponse (setResponseCode code) 
  writeBS (Char8.pack msg)

-- TODO [5/10/2015]: We should be returning the location. And (maybe) checking if
--                   the value is really new.
created :: MonadSnap m => m ()
created = modifyResponse $ setResponseCode 201

ok :: MonadSnap m => m ()
ok = modifyResponse $ setResponseCode 200

key = SecretKey "sk_test_uC4GpgWwiU0Bo6nHbE5Tnc9i"
stripeConfig = StripeConfig key "" V20110915d

dollars = Currency "usd"

amountToCharge :: Membership -> Amount
amountToCharge = Amount . (*100) . _cost

charge :: AcidSnap CSA ()
charge =  method GET (failWith 405 "Method GET not supported for this URI")
            <|> method POST charge'
  where
    charge' = withRequest (recordAndCharge . rqParams) >> return ()
    recordAndCharge (makeMembership -> Just (m,tid) ) = do
      st <- ask
      liftIO . putStrLn $ "Received charge: " ++ show m
      res <- runStripeT stripeConfig $
        chargeTokenById tid (amountToCharge m) dollars Nothing Nothing 
      case res of
        Left failure -> do
          errLog $ "Stripe Failure: " ++ show failure
        Right charge -> do
          update' st . NewMembership $ m { _payment = Just charge }
          liftIO . putStrLn $ "Charge succeeded: " ++ show charge

    recordAndCharge params = do
      logError $ "Bad params: " <> (Char8.pack.show) params
      failWith 401 "Bad request"

membare :: Membership
membare = Membership "" [] (Fall2015 (Fall 0 0 0)) 0 Nothing

makeMembership :: Params -> Maybe (Membership, TokenId)
makeMembership params = do
  let readBS :: Read a => [ByteString] -> [a]
      readBS = fmap (read . Char8.unpack)
  [tid] <- fmap (TokenId . decodeUtf8) <$> Map.lookup "stripeToken" params
  [nm]  <- fmap Char8.unpack <$> Map.lookup "shareholder" params
  [veg] <- readBS <$> Map.lookup "vegetableShares" params
  [egg] <- readBS <$> Map.lookup "eggShares" params
  [frt] <- readBS <$> Map.lookup "fruitShares" params
  [p]   <- readBS <$> Map.lookup "cost" params
  return $ ( Membership { _shareholder = nm
                        , _alternates  = []
                        , _season = Fall2015 (Fall { _vegetable = veg
                                                   , _egg   = egg
                                                   , _fruit = frt
                                                   })
                        , _cost = p
                        , _payment = Nothing
                        }
           , tid )
     
errLog :: MonadSnap m => String -> m ()
errLog = logError . Char8.pack 
{-
snapUpdate :: (Serialize a, UpdateEvent u, MethodState u ~ st)
           => (a -> u) -> AcidSnap st ()
snapUpdate uf = do
  state <- ask
  bs    <- readRequestBody 2000
  case decodeLazy bs of
    Left  str -> failWith 400 (printf "Bad post val: %s\nwhile decoding: %s" str (show bs))     
    Right val -> update' state (uf val) >> created

snapSend :: (QueryEvent q, MethodState q ~ st, Serialize (MethodResult q))
         => q -> AcidSnap st ()
snapSend q = do
  state <- ask
  bytes <- encodeLazy <$> query' state q
  writeLBS bytes
  ok

addName :: AcidSnap GraphState ()
addName = snapUpdate PutName
  
sendNames :: AcidSnap GraphState ()
sendNames = snapSend GetNames

handleNames :: AcidSnap GraphState ()
handleNames = method GET sendNames <|> method POST addName

addIdentification :: AcidSnap GraphState ()
addIdentification = snapUpdate PutIdentification
  
sendIdentifications :: AcidSnap GraphState ()
sendIdentifications = snapSend GetIdentifications

handleIdentifications :: AcidSnap GraphState ()
handleIdentifications =
  method GET sendIdentifications <|>
  method POST addIdentification

addRelationship :: AcidSnap GraphState ()
addRelationship = snapUpdate PutRelationship
  
sendRelationships :: AcidSnap GraphState ()
sendRelationships = snapSend GetRelationships

handleRelationships :: AcidSnap GraphState ()
handleRelationships =
  method GET sendRelationships <|>
  method POST addRelationship
-}
