{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections,
             TypeFamilies, FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, ViewPatterns, TemplateHaskell  #-}
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.Reader
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (decodeUtf8)
import           Debug.Trace
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Text.Printf (printf)
import           Web.Stripe.Charge
import           Web.Stripe.Client (SecretKey(..), StripeVersion(..))
import           Web.Stripe.Token
import           Acid
import           Types

data Options = Options { _dataDir   :: FilePath
                       , _clientDir :: FilePath
                       -- , port      :: Int
                       }

makeLenses ''Options

-- Stick our options into the "other" field of snap's Config as
-- a function that transforms our Options type.
options :: [ OptDescr (Maybe (Config Snap (Options -> Options))) ]
options =
    [ Option "d" ["dataDir"]   (ReqArg dd "DATADIR")
            "Acid-state log location"
    , Option "i" ["clientDir"] (ReqArg cd "CLIENTCODE")
            "Client-side code location"
    ]
  where
    dd dir = Just $ setOther (dataDir   .~ dir) mempty
    cd dir = Just $ setOther (clientDir .~ dir) mempty

defOpts = Options "./state" "./webform.jsexe"

parseArgs :: IO (Config Snap (Options -> Options))
parseArgs = extendedCommandLineConfig (options ++ optDescrs bc) (.) bc
  where
    bc :: Config Snap a
    bc = mempty

main :: IO ()
main = do
  cfg  <- parseArgs
  let opts = fromMaybe id (getOther cfg) $ defOpts
  bracket
    (openLocalStateFrom (_dataDir opts) (CSA mempty))
    createCheckpointAndClose
    (httpServe cfg . runReaderT (site $ _clientDir opts))

site :: FilePath -> AcidSnap CSA ()
site root =
    route [ ("charge",            charge )
          , ("css",               serveDirectory $ root ++ "/css")
          ] <|>
    serveDirectory (root ++ "/bin/paisley-client.jsexe")

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

readKey :: MonadIO m => m SecretKey
readKey = liftIO $ SecretKey . T.takeWhile (/= '\n') <$> T.readFile "stripe.key"

--key = SecretKey "sk_test_qPudtS2Y66BMCSE7SeM3NcnC"
stripeConfig :: SecretKey -> StripeConfig
stripeConfig key = StripeConfig key "" V20110915d

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
      key <- readKey
      res <- runStripeT (stripeConfig key) $
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
membare = Membership "" "111-1111" "test@test.net" [] (Summer2016 (Fall 0 0 0)) 0 Nothing

makeMembership :: Params -> Maybe (Membership, TokenId)
makeMembership params = do
    [tid] <- fmap (TokenId . decodeUtf8) <$> Map.lookup "stripeToken" params
    [nm]  <- fmap Char8.unpack <$> Map.lookup "shareholder" params
    [s]   <- fmap Char8.unpack <$> Map.lookup "season" params
    [a]   <- fmap (lines . Char8.unpack) <$> Map.lookup "alternates" params
    [e]   <- fmap Char8.unpack <$> Map.lookup "email" params
    [ph]  <- fmap Char8.unpack <$> Map.lookup "phone" params
    seas  <- case s of
              "Fall2015"   -> fall
              "Winter2016" -> winter
              "Summer2016" -> fall
              _            -> fail "no season specified"
    [p]   <- readBS <$> Map.lookup "cost" params
    return $ ( Membership { _shareholder = nm
                          , _alternates  = a
                          , _email = e
                          , _phone = ph
                          , _season = seas
                          , _cost = p
                          , _payment = Nothing
                          }
             , tid )
  where
    readBS :: Read a => [ByteString] -> [a]
    readBS = fmap (read . Char8.unpack)
    fall = do
      [veg] <- readBS <$> Map.lookup "vegetableShares" params
      [egg] <- readBS <$> Map.lookup "eggShares" params
      [frt] <- readBS <$> Map.lookup "fruitShares" params
      return $ Summer2016 (Fall { _vegetable = veg
                                , _egg   = egg
                                , _fruit = frt
                                })
    winter = do
      [win] <- readBS <$> Map.lookup "vegetableShares" params
      [egg] <- readBS <$> Map.lookup "eggShares" params
      return $ Winter2016 (Winter { _winter = win
                                  , _wegg   = egg
                                  })

errLog :: MonadSnap m => String -> m ()
errLog = logError . Char8.pack
