module Selenium.Builder
  ( build
  , browser
  , version
  , platform
  , usingServer
  , scrollBehaviour
  , options
  , withCapabilities
  , Build
  ) where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(..), singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Selenium.Browser (Browser, browserCapabilities, platformCapabilities, versionCapabilities)
import Selenium.Capabilities (Capabilities, emptyCapabilities)
import Selenium.Options (Options)
import Selenium.Types (Builder, ControlFlow, Driver, LoggingPrefs, ProxyConfig, ScrollBehaviour)

data Command
  = SetOptions Options
  | SetControlFlow ControlFlow
  | SetEnableNativeEvents Boolean
  | SetLoggingPrefs LoggingPrefs
  | SetProxy ProxyConfig
  | SetScrollBehaviour ScrollBehaviour
  | UsingServer String

newtype Build a = Build (Writer (Tuple Capabilities (List Command)) a)

unBuild ∷ ∀ a. Build a → Writer (Tuple Capabilities (List Command)) a
unBuild (Build a) = a

instance functorBuild ∷ Functor Build where
  map f (Build a) = Build $ f <$> a

instance applyBuild ∷ Apply Build where
  apply (Build f) (Build w) = Build $ f <*> w

instance bindBuild ∷ Bind Build where
  bind (Build w) f = Build $ w >>= unBuild <<< f

instance applicativeBuild ∷ Applicative Build where
  pure = Build <<< pure

instance monadBuild ∷ Monad Build

rule ∷ Command → Build Unit
rule = Build <<< tell <<< Tuple emptyCapabilities <<< singleton

version ∷ String → Build Unit
version = withCapabilities <<< versionCapabilities

platform ∷ String → Build Unit
platform = withCapabilities <<< platformCapabilities

usingServer ∷ String → Build Unit
usingServer = rule <<< UsingServer

scrollBehaviour ∷ ScrollBehaviour → Build Unit
scrollBehaviour = rule <<< SetScrollBehaviour

options :: Options -> Build Unit
options = rule <<< SetOptions

withCapabilities ∷ Capabilities → Build Unit
withCapabilities c = Build $ tell $ Tuple c noRules
  where
  noRules ∷ List Command
  noRules = Nil

browser ∷ Browser → Build Unit
browser = withCapabilities <<< browserCapabilities

build ∷ Build Unit → Aff Driver
build dsl = do
  builder ← fromEffectFnAff _newBuilder
  case execWriter $ unBuild dsl of
    Tuple capabilities commands →
      fromEffectFnAff $ _build $ runFn2 _withCapabilities (interpret commands builder) capabilities

interpret ∷ List Command → Builder → Builder
interpret commands initialBuilder = foldl foldFn initialBuilder commands
  where
  foldFn ∷ Builder → Command → Builder
  foldFn b (UsingServer s) = runFn2 _usingServer b s
  foldFn b (SetScrollBehaviour bh) = runFn2 _setScrollBehaviour b bh
  foldFn b (SetOptions opts) = runFn2 _setOptions b opts
  foldFn b _ = b


foreign import _newBuilder ∷ EffectFnAff Builder
foreign import _build ∷ Builder → EffectFnAff Driver

foreign import _usingServer ∷ Fn2 Builder String Builder
foreign import _setScrollBehaviour ∷ Fn2 Builder ScrollBehaviour Builder
foreign import _withCapabilities ∷ Fn2 Builder Capabilities Builder
foreign import _setOptions ∷ Fn2 Builder Options Builder
