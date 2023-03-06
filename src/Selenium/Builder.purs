module Selenium.Builder
  ( build
  --, browser
  --, version
  --, platform
  , usingServer
  , scrollBehaviour
  , options
  , Build
  , Command
  ) where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)
import Data.Exists (mkExists)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(..), foldM, singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Selenium.Browser (Browser, browserCapabilities, platformCapabilities, versionCapabilities)
import Selenium.Capabilities (Capabilities, emptyCapabilities)
import Selenium.Options (Options, Options')
import Selenium.Types (Builder, ControlFlow, Driver, LoggingPrefs, ProxyConfig, ScrollBehaviour)

data Command
  = SetOptions Options
  | SetControlFlow ControlFlow
  | SetEnableNativeEvents Boolean
  | SetLoggingPrefs LoggingPrefs
  | SetProxy ProxyConfig
  | SetScrollBehaviour ScrollBehaviour
  | UsingServer String

-- TODO: make this a newtype again to fix the import
type Build a = Writer (List Command) a

rule ∷ Command → Build Unit
rule = tell <<< pure

--version ∷ String → Build Unit
--version = withCapabilities <<< versionCapabilities
--
--platform ∷ String → Build Unit
--platform = withCapabilities <<< platformCapabilities
--
--withCapabilities ∷ Capabilities → Build Unit
--withCapabilities c = Build $ tell $ Tuple c noRules

usingServer ∷ String → Build Unit
usingServer = rule <<< UsingServer

scrollBehaviour ∷ ScrollBehaviour → Build Unit
scrollBehaviour = rule <<< SetScrollBehaviour

options :: forall r. Options' r -> Build Unit
options opts = rule (SetOptions (mkExists opts))

--browser ∷ Browser → Build Unit
--browser = withCapabilities <<< browserCapabilities

build ∷ Browser -> Build Unit → Aff Driver
build browser dsl = do
  newBuilder ← fromEffectFnAff _newBuilder
  let capabilities = browserCapabilities browser
  let builder = runFn2 _withCapabilities (interpret (execWriter dsl) newBuilder) capabilities
  fromEffectFnAff $ _build $ builder

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
