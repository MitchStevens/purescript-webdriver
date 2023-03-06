module Selenium.Capabilities where

import Prelude

import Data.Maybe (Maybe(..))

foreign import data Capabilities ∷ Type
foreign import emptyCapabilities ∷ Capabilities
foreign import appendCapabilities ∷ Capabilities → Capabilities → Capabilities

foreign import deleteCapability
  :: String
  -> Capabilities
  -> Capabilities

foreign import _getCapability
  :: forall a
  . Maybe a
  -> (a -> Maybe a)
  -> String
  -> Capabilities
  -> Maybe String

getCapability 
  :: String
  -> Capabilities
  -> Maybe String
getCapability = _getCapability Nothing Just

foreign import setCapability
  :: String
  -> String
  -> Capabilities
  -> Capabilities

foreign import hasCapability
  :: String
  -> Capabilities
  -> Boolean

instance semigroupCapabilities ∷ Semigroup Capabilities where
  append = appendCapabilities

instance monoidCapabilities ∷ Monoid Capabilities where
  mempty = emptyCapabilities