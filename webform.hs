{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell, OverloadedStrings  #-}

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

stripeKey :: String
stripeKey = "pk_test_6zhuiwgEyXlswkfcKGT61PAy"

vegPrice,eggPrice,fruitPrice :: Int
vegPrice   = 300
eggPrice   = 30 -- counting by half-shares 
fruitPrice = 150

vegAttr :: forall t. Reflex t => Dynamic t (Map String String)
vegAttr = constDyn $ "name" =: "vegetableShares"

vegOpts = 
  1   =: "One share" <>
  2   =: "Two shares"

fruitAttr :: forall t. Reflex t => Dynamic t (Map String String)
fruitAttr = constDyn $ "name" =: "fruitShares"

fruitOpts = 
  0   =: "None" <>
  1   =: "One share" <>
  2   =: "Two shares"

eggAttr :: forall t. Reflex t => Dynamic t (Map String String)
eggAttr = constDyn $ "name" =: "eggShares"

eggOpts :: Map Int String
eggOpts = 
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

nameInputConf :: forall t. Reflex t => TextAreaConfig t 
nameInputConf = def 
  { _textAreaConfig_attributes = constDyn ( "name" =: "shareholder") }

infixl 4 <<$>>
(<<$>>) :: (Reflex t, MonadHold t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
(<<$>>) = mapDyn

infixl 4 <<*>>
(<<*>>) :: (Reflex t, MonadHold t m) => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
(<<*>>) mf v = mf >>= \f -> combineDyn ($) f v  

-- Workaround for my old reflex-dom. Also, we only need the dyn value.
dropdown'' :: forall k t m. (MonadWidget t m, Ord k, Show k, Read k)
                => k -> Map k String -> Dynamic t (Map String String)
                -> m (Dynamic t k)
dropdown'' k opts attr = _dropdown_value <$>
  dropdownDynAttr attr k never (constDyn opts)

label :: MonadWidget t m => String -> m a -> m a
label l i = el "p" . el "label" $ text l >> i
  
br :: MonadWidget t m => m ()
br = el "br" $ return ()

main = mainWidgetWithCss $(embedFile "style.css") $ do
  el "h1" $ text "Purchase shares"
  elAttr "form" ( "action" =: "/charge" <> "method" =: "POST") $ do
    name    <- label "Your name:" $ _textArea_value <$> 
        textArea nameInputConf 
    veggies <- label "Vegetable shares: " $
        mapDyn (vegPrice *)   =<< dropdown'' 1 vegOpts   vegAttr
    eggs    <- label "Egg shares:       " $
        mapDyn (eggPrice *)   =<< dropdown'' 0 eggOpts   eggAttr 
    fruit   <- label "Fruit share:      " $
        mapDyn (fruitPrice *) =<< dropdown'' 0 fruitOpts fruitAttr

    let costAttrs c = Map.fromList
                       [("type", "hidden"), ("name", "cost"), ("value", show c)]
    charge  <- (\ x y z -> x + y + z) <<$>> veggies <<*>> eggs <<*>> fruit 
    -- Hidden submission field
    chargeAsAttrs <- costAttrs <<$>> charge
    elDynAttr "input" chargeAsAttrs $ return ()
    -- Visible, non-submitted running total
    el "p" $ dynText =<< mapDyn (\c -> ("Total: $" ++ show c ++ ".00")) charge

    checkout charge
