module Selenium.Combinators where

import Prelude
import Control.Alt ((<|>))
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, isJust, maybe)
import Data.Time.Duration (class Duration, Milliseconds(..))
import Selenium.Monad (Selenium, getCurrentUrl, wait, attempt, findExact, tryRepeatedlyTo, tryRepeatedlyTo', findElement, byCss, later, byClassName, byName, byId, byXPath)
import Selenium.Types (Element, Locator)

-- | Retry computation until it successed but not more then `n` times
retry ∷ ∀ o a. Int → Selenium o a → Selenium o a
retry n action = do
  res ← attempt action
  case res of
    Left e
      | n > one → retry (n - one) action
      | otherwise → lift $ throwError $ error "To many retries"
    Right r → pure r

-- | Tries to find element by string checks: css, xpath, id, name and classname
tryFind ∷ ∀ o. String → Selenium o Element
tryFind probablyLocator =
  (byCss probablyLocator >>= findExact) <|>
  (byXPath probablyLocator >>= findExact) <|>
  (byId probablyLocator >>= findExact) <|>
  (byName probablyLocator >>= findExact) <|>
  (byClassName probablyLocator >>= findExact)

waitUntilJust ∷ ∀ d o a. Duration d ⇒ Selenium o (Maybe a) → d → Selenium o a
waitUntilJust check time = do
  wait (checker $ isJust <$> check) time
  check >>= maybe (throwError $ error $ "Maybe was not Just after waiting for isJust") pure

-- Tries to evaluate `Selenium` if it returns `false` after 500ms
checker ∷ ∀ o. Selenium o Boolean → Selenium o Boolean
checker check =
  check >>= if _
    then pure true
    else later (Milliseconds 500.0) $ checker check

getElementByCss ∷ ∀ o. String → Selenium o Element
getElementByCss cls =
  byCss cls
    >>= findElement
    >>= maybe (throwError $ error $ "There is no element matching css: " <> cls) pure

checkNotExistsByCss ∷ ∀ o. String → Selenium o Unit
checkNotExistsByCss = contra <<< getElementByCss

contra ∷ ∀ o a. Selenium o a → Selenium o Unit
contra check = do
  eR ← attempt check
  either
    (const $ pure unit)
    (const $ throwError $ error "check successed in contra") eR

-- | Repeatedly attempts to find an element using the provided selector until the
-- | provided timeout elapses.
tryToFind' ∷ ∀ d o. Duration d ⇒ d → Selenium o Locator → Selenium o Element
tryToFind' timeout locator = tryRepeatedlyTo' timeout $ locator >>= findExact

-- | Repeatedly tries to find an element using the provided selector until
-- | the provided `Selenium`'s `defaultTimeout` elapses.
tryToFind ∷ ∀ o. Selenium o Locator → Selenium o Element
tryToFind locator = tryRepeatedlyTo $ locator >>= findExact

-- | Repeatedly tries to evaluate check (third arg) for timeout ms (first arg)
-- | finishes when check evaluates to true.
-- | If there is an error during check or it constantly returns `false`
-- | throws error with message (second arg)
await ∷ ∀ d o. Duration d ⇒ d → Selenium o Boolean → Selenium o Unit
await timeout check = do
  ei ← attempt $ wait (checker check) timeout
  case ei of
    Left _ → throwError $ error "await has no success"
    Right _ → pure unit

awaitUrlChanged ∷ ∀ o. String → Selenium o Boolean
awaitUrlChanged oldURL = checker $ (oldURL /= _) <$> getCurrentUrl
