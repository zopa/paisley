{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell, OverloadedStrings,
             OverloadedLists #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Default
import Reflex
import Reflex.Dom
import Data.FileEmbed
import Data.Text (Text)
import Data.Text (pack)

-- Publishable (ie, public) key.
stripeKey :: String
stripeKey = "pk_test_8rfCfP81ykqIHEZ0yrMKgSPx"

vegPrice,eggPrice,fruitPrice :: Int
vegPrice   = 300 -- counting by half shares
eggPrice   =  54 -- counting by half shares
fruitPrice = 270

wvegPrice,weggPrice :: Int
wvegPrice = 300
weggPrice = 18 -- Per half-share

vegAttr :: forall t. Reflex t => DropdownConfig t Int
vegAttr = DropdownConfig never (constDyn $ "name" =: "vegetableShares")

vegOpts :: Reflex t => Dynamic t (Map Int String)
vegOpts =  constDyn $
  1   =: "Half share" <>
  2   =: "One share"  <>
  4   =: "Two shares"

fruitAttr :: forall t. Reflex t => DropdownConfig t Int
fruitAttr = DropdownConfig never (constDyn $ "name" =: "fruitShares")

fruitOpts :: Reflex t => Dynamic t (Map Int String)
fruitOpts = constDyn $
  0   =: "None" <>
  1   =: "One share" <>
  2   =: "Two shares"

eggAttr :: forall t. Reflex t => DropdownConfig t Int
eggAttr = DropdownConfig never (constDyn $ "name" =: "eggShares")

eggOpts :: Reflex t => Dynamic t (Map Int String)
eggOpts =  constDyn $
  0 =: "None" <>
  1 =: "Half share (6 eggs/week)" <>
  2 =: "Full share (12 eggs/week)" <>
  4 =: "Two shares" <>
  6 =: "Three shares"

checkout :: (Reflex t, MonadWidget t m) => Dynamic t Int -> m ()
checkout total = do
  let staticAttrs = "src" =: "https://checkout.stripe.com/checkout.js" <>
                    "class" =: "stripe-button" <>
                    "data-key" =: stripeKey <>
                    -- "data-image" =: "/img/documentation/checkout/marketplace.png" <>
                    "data-name" =: "Paisly Farm Demo" <>
                    "data-description" =: "" <>
                    "data-locale" =: "auto" <>
                    "data-panel-label" =: "Buy Shares"
  attrs <- mapDyn (\t -> staticAttrs) total
  elDynAttr "script" attrs $ return ()

nameInputConf :: forall t. Reflex t => TextInputConfig t
nameInputConf = def
  { _textInputConfig_attributes = constDyn ( "name" =: "shareholder") }

altsInputConf :: forall t. Reflex t => TextAreaConfig t
altsInputConf = def
  { _textAreaConfig_attributes = constDyn ( "name" =: "alternates") }

phoneInputConf :: forall t. Reflex t => TextInputConfig t
phoneInputConf = def
  { _textInputConfig_attributes = constDyn ( "name" =: "phone") }

emailInputConf :: forall t. Reflex t => TextInputConfig t
emailInputConf = def
  { _textInputConfig_attributes = constDyn ( "name" =: "email") }

infixl 4 <<$>>
(<<$>>) :: (Reflex t, MonadHold t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
(<<$>>) = mapDyn

infixl 4 <<*>>
(<<*>>) :: (Reflex t, MonadHold t m) => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
(<<*>>) mf v = mf >>= \f -> combineDyn ($) f v

label l i = el "p" . el "label" $ text l >> i

labelbr :: MonadWidget t m => String -> m a -> m a
labelbr l i = el "p" . el "label" $ text l >> el "br" (return ()) >> i

hidden :: (Reflex t, MonadWidget t m) =>
          String -> Dynamic t (Map String String) -> m a -> m a
hidden str dattrs m = el "p" $ do
   dattrs' <- Map.insert "type" "hidden" <<$>> dattrs
   elDynAttr str dattrs' m

br :: MonadWidget t m => m ()
br = el "br" $ return ()


stylesheet :: MonadWidget t m => String -> Map String String -> m ()
stylesheet href attrs = elAttr "link" attrs' $ return ()
  where attrs' = attrs <> Map.fromList [("rel","stylesheet"),("href",href)]

a :: MonadWidget t m => String -> Map String String -> m a ->  m a
a href attrs = elAttr "a" ("href" =: href <> attrs)

a_ :: MonadWidget t m => String -> m a ->  m a
a_ = flip a mempty

img :: MonadWidget t m => String -> Map String String -> m ()
img href attrs = elAttr "img" ("href" =: href <> attrs) $ return ()

nav :: MonadWidget t m => [ m () ] -> m ()
nav links = sequence_ links

headEl :: MonadWidget t m => m () --Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) ()
headEl = do
  stylesheet "/css/style.css" mempty
  stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    ("integrity" =: "sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" <>
     "crossorigin" =: "anonymous")

main = mainWidgetWithHead headEl $ do
  el "header" $ do
    nav [ a_ "http://upstatefarmsny.com/home.html" $ text "HOME |"
        , a_ "http://upstatefarmsny.com/our-history.html" $ text " Our History |"
        , a_ "http://upstatefarmsny.com/restaurant-services" $ text " Restaurant Products & Services |"
        , a_ "http://upstatefarmsny.com/paisleyfarm.html" $ text " Paisley Farm & CSA |"
        , a_ "http://upstatefarmsny.com/contact.html" $ text " Contact Us |"
        , a_ "http://upstatefarmsny.com/Blog.html" $ text " Blog"
        ]
    img "http://upstatefarmsny.com/images/minimalismheader.jpg" mempty
    el "hr" $ return ()
  el "h1" $ text "Purchase shares"
  el "h2" $ text "Demo"
  elAttr "form" ( "action" =: "/charge" <> "method" =: "POST") $ do
    name    <- fmap value . label "Your name:" $ textInput nameInputConf
    alts    <- fmap value . labelbr "Other people who can pick up your share:"
             $ textArea  altsInputConf
    email   <- fmap value . label "Email:" $ textInput emailInputConf
    phone   <- fmap value . label "Phone Number:" $ textInput phoneInputConf
    veggies <- label "Vegetable shares: " $
        mapDyn (vegPrice *) . value =<< dropdown 1 vegOpts vegAttr
    eggs    <- label "Egg shares:       " $
        mapDyn (eggPrice *) . value =<< dropdown 0 eggOpts eggAttr
    fruit   <- label "Fruit share:      " $
        mapDyn (fruitPrice *) . value =<< dropdown 0 fruitOpts fruitAttr

    let costAttrs c = Map.fromList
                       [("type", "hidden"), ("name", "cost"), ("value", show c)]
    seasonAttrs <- return . constDyn $ Map.fromList
                       [("type", "hidden"), ("name", "season"), ("value", "Summer2016")]
    charge  <- (\ x y z -> x + y + z) <<$>> veggies <<*>> eggs <<*>> fruit
    -- Hidden submission field

    chargeAsAttrs <- costAttrs <<$>> charge
    hidden "input" seasonAttrs   $ return ()
    hidden "input" chargeAsAttrs $ return ()
    -- Visible, non-submitted running total
    el "p" $ dynText =<< mapDyn (\c -> ("Total: $" ++ show c ++ ".00")) charge

    checkout charge
  br >> br
  el "p" $ text "You can test this form with these fake credit cards:"
  el "ul" $ do
    el "li" $ text "4242 4242 4242 4242 (Visa)"
    el "li" $ text "5555 5555 5555 4444 (MasterCard)"
