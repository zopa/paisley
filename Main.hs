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
import           Debug.Trace
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Text.Printf (printf)

-- import           Acid
-- import           Types

main :: IO ()
main = quickHttpServe site --bracket
--  (openLocalState initialGraph)
--  createCheckpointAndClose
--  (quickHttpServe . runReaderT site )

site :: Snap ()
site =
    route [ ("charge",            charge )
  --        , ("identifications",   handleIdentifications)
  --        , ("relationships",     handleRelationships)
  --        , ("resources",         serveDirectory "resources")
          ] <|>
    serveDirectory "webform.jsexe"

--type AcidSnap st = ReaderT (AcidState st) Snap

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

charge :: MonadSnap m => m ()
charge =  method GET (failWith 405 "Method GET not supported for this URI")
            <|> method POST charge'
  where
    charge' = withRequest (recordAndCharge . rqParams) >> return ()
    recordAndCharge (makeMembership -> Just m ) =
      liftIO . putStrLn $ "Recieved charge: " ++ show m
    recordAndCharge params = do
      logError $ "Bad params: " <> (Char8.pack.show) params
      failWith 401 "Bad request"

data Membership = Membership { shareholder :: String
                             , vegShares   :: Int
                             , eggShares   :: Int
                             , fruitShares :: Int
                             , cost        :: Int
                             } deriving (Eq, Ord, Show)
      
membare :: Membership
membare = Membership "" 0 0 0 0

makeMembership :: Params -> Maybe Membership
makeMembership params = do
  let readBS :: Read a => String -> [ByteString] -> [a]
      readBS msg = trace msg $ fmap (read . Char8.unpack)
  [nm  :: String] <- fmap Char8.unpack <$> Map.lookup "shareholder" params
  [veg :: Int] <- readBS "veg\n" <$> Map.lookup "vegetableShares" params
  [egg :: Int] <- readBS "eggs, precious\n"<$> Map.lookup "eggShares" params
  [frt :: Int] <- readBS "nasty fruit\n" <$> Map.lookup "fruitShares" params
  [p   :: Int] <- readBS "precious\n" <$> Map.lookup "cost" params
  return $ Membership nm veg egg frt p
      
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
