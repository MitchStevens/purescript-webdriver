-- | Most functions of `Selenium` use `Driver` as an argument
-- | This module supposed to make code a bit cleaner through
-- | putting `Driver` to `ReaderT`
module Selenium.Monad
  where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader.Trans (ReaderT(..), lift, ask, runReaderT)
import Data.Either (Either)
import Data.Int as Int
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import Effect.Aff (Aff, delay, forkAff, supervise)
import Effect.Aff as A
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (new, read, write)
import Foreign (Foreign)
import Selenium as S
import Selenium.ActionSequence as AS
import Selenium.Types (Driver, Element, FileDetector, Location, Locator, Size, Window, WindowHandle, XHRStats)
import Selenium.XHR as XHR

-- | `Driver` is field of `ReaderT` context
-- | Usually selenium tests are run with tons of configs (i.e. xpath locators,
-- | timeouts) all those configs can be putted to `Selenium e o a`
type Selenium a =
  ReaderT { driver ∷ Driver, defaultTimeout ∷ Milliseconds } Aff a

-- | get driver from context
getDriver ∷ Selenium Driver
getDriver = _.driver <$> ask

getWindow ∷ Selenium Window
getWindow =  getDriver >>= S.getWindow >>> lift

getWindowPosition ∷ Selenium Location
getWindowPosition = getWindow >>= S.getWindowPosition >>> lift

getWindowSize ∷ Selenium Size
getWindowSize = getWindow >>= S.getWindowSize >>> lift

maximizeWindow ∷ Selenium Unit
maximizeWindow = getWindow >>= lift <<< S.maximizeWindow

setWindowPosition ∷ Location -> Selenium Unit
setWindowPosition pos = getWindow >>= S.setWindowPosition pos >>> lift

setWindowSize ∷ Size -> Selenium Unit
setWindowSize size = getWindow >>= S.setWindowSize size >>> lift

getWindowScroll ∷ Selenium Location
getWindowScroll = getDriver >>= S.getWindowScroll >>> lift

-- LIFT `Aff` combinators to `Selenium.Monad`
apathize ∷ ∀ a. Selenium a -> Selenium Unit
apathize check = ReaderT \r ->
  A.apathize $ runReaderT check r

attempt ∷ ∀ a. Selenium a -> Selenium (Either Error a)
attempt check = ReaderT \r ->
  A.attempt $ runReaderT check r

later ∷ ∀ d a. Duration d => d -> Selenium a -> Selenium a
later time check = 
  lift (A.delay (fromDuration time)) *> check

-- LIFT `Selenium` funcs to `Selenium.Monad`
get ∷ String -> Selenium Unit
get url =
  getDriver >>= lift <<< flip S.get url

wait ∷ ∀ d. Duration d => Selenium Boolean -> d -> Selenium Unit
wait check time = ReaderT \r ->
  S.wait (runReaderT check r) (fromDuration time) r.driver

-- | Tries the provided Selenium computation repeatedly until the provided timeout expires
tryRepeatedlyTo' ∷ ∀ d a. Duration d => d -> Selenium a -> Selenium a
tryRepeatedlyTo' time selenium = ReaderT \r ->
  reattempt (fromDuration time) (runReaderT selenium r)
  where
    reattempt ∷ Milliseconds -> Aff a -> Aff a
    reattempt ms aff = supervise do
      elapsed ← liftEffect $ new false
      _ ← forkAff do
        delay ms
        liftEffect $ write true elapsed
      let attempt = aff `catchError` \error -> do
            shouldRethrow ← liftEffect $ read elapsed
            if shouldRethrow
              then throwError error
              else attempt
      attempt

-- -- | Tries the provided Selenium computation repeatedly until `Selenium`'s defaultTimeout expires
tryRepeatedlyTo ∷ ∀ a. Selenium a -> Selenium a
tryRepeatedlyTo selenium = ask >>= \r ->
  tryRepeatedlyTo' r.defaultTimeout selenium

byCss ∷ String -> Selenium Locator
byCss = lift <<< S.byCss

byXPath ∷ String -> Selenium Locator
byXPath = lift <<< S.byXPath

byId ∷ String -> Selenium Locator
byId = lift <<< S.byId

byName ∷ String -> Selenium Locator
byName = lift <<< S.byName

byClassName ∷ String -> Selenium Locator
byClassName = lift <<< S.byClassName

byDataAutomation :: String -> Selenium Locator
byDataAutomation dataAutomationTag =
  byXPath (".//*[@data-automation='" <> dataAutomationTag <>"']")

-- | get element by action returning an element
-- | ```purescript
-- | locator \el -> do
-- |   commonElements ← byCss ".common-element" >>= findElements el
-- |   flaggedElements ← traverse (\el -> Tuple el <$> isVisible el) commonElements
-- |   maybe err pure $ foldl foldFn Nothing flaggedElements
-- |   where
-- |   err = throwError $ error "all common elements are not visible"
-- |   foldFn Nothing (Tuple el true) = Just el
-- |   foldFn a _ = a
-- | ```
--locator ∷ (Element -> Selenium Element) -> Selenium Locator
--locator checkFn = ReaderT \r ->
--
--  --S.affLocator  :: (Element -> Aff Element) -> Aff Locator
--
--  ReaderT \r ->
--  S.affLocator (\el -> runReaderT (checkFn el) r)

-- | Tries to find element and return it wrapped in `Just`
findElement ∷ Locator -> Selenium (Maybe Element)
findElement l = do
  getDriver >>= lift <<< flip S.findElement l

findElements ∷ Locator -> Selenium (List Element)
findElements l =
  getDriver >>= lift <<< flip S.findElements l

-- | Tries to find child and return it wrapped in `Just`
findChild ∷ Element -> Locator -> Selenium (Maybe Element)
findChild el loc = lift $ S.findChild el loc

findChildren ∷ Element -> Locator -> Selenium (List Element)
findChildren el loc = lift $ S.findChildren el loc

getInnerHtml ∷ Element -> Selenium String
getInnerHtml = lift <<< S.getInnerHtml

getSize ∷ Element -> Selenium Size
getSize = lift <<< S.getSize

getLocation ∷ Element -> Selenium Location
getLocation = lift <<< S.getLocation

isDisplayed ∷ Element -> Selenium Boolean
isDisplayed = lift <<< S.isDisplayed

isEnabled ∷ Element -> Selenium Boolean
isEnabled = S.isEnabled >>> lift

getCssValue ∷ String -> Element -> Selenium String
getCssValue key el = lift $ S.getCssValue el key

getAttribute ∷ String -> Element -> Selenium (Maybe String)
getAttribute attr el = lift $ S.getAttribute el attr

getText ∷ Element -> Selenium String
getText el = lift $ S.getText el

clearEl ∷ Element -> Selenium Unit
clearEl = lift <<< S.clearEl

clickEl ∷ Element -> Selenium Unit
clickEl = lift <<< S.clickEl

sendKeysEl ∷ String -> Element -> Selenium Unit
sendKeysEl ks el = lift $ S.sendKeysEl ks el

script ∷ String -> Selenium Foreign
script str =
  getDriver >>= flip S.executeStr str >>> lift

getCurrentUrl ∷ Selenium String
getCurrentUrl = getDriver >>= S.getCurrentUrl >>> lift

navigateBack ∷ Selenium Unit
navigateBack = getDriver >>= S.navigateBack >>> lift

navigateForward ∷ Selenium Unit
navigateForward = getDriver >>= S.navigateForward >>> lift

navigateTo ∷ String -> Selenium Unit
navigateTo url = getDriver >>= S.navigateTo url >>> lift

setFileDetector ∷ FileDetector -> Selenium Unit
setFileDetector fd = getDriver >>= flip S.setFileDetector fd >>> lift

getTitle ∷ Selenium String
getTitle = getDriver >>= S.getTitle >>> lift


-- | Run sequence of actions
sequence ∷ AS.Sequence Unit -> Selenium Unit
sequence seq = do
  getDriver >>= lift <<< flip AS.sequence seq

-- | Same as `sequence` but takes function of `ReaderT` as an argument
actions ∷ ({ driver ∷ Driver, defaultTimeout ∷ Milliseconds } -> AS.Sequence Unit)
  -> Selenium Unit
actions seqFn = do
  ctx ← ask
  sequence $ seqFn ctx

-- | Stop computations
stop ∷ Selenium Unit
stop = wait (later msMax $ pure false) msMax
  where
  msMax = Milliseconds (Int.toNumber (top ∷ Int))

refresh ∷ Selenium Unit
refresh = getDriver >>= S.refresh >>> lift

quit ∷ Selenium Unit
quit = getDriver >>= S.quit >>> lift

takeScreenshot ∷ Selenium String
takeScreenshot = getDriver >>= S.takeScreenshot >>> lift

saveScreenshot ∷ String -> Selenium Unit
saveScreenshot name = getDriver >>= S.saveScreenshot name >>> lift

-- | Tries to find element, if has no success throws an error
findExact ∷ Locator -> Selenium Element
findExact loc = getDriver >>= flip S.findExact loc >>> lift

-- | Tries to find element and throws an error if it succeeds.
loseElement ∷ Locator -> Selenium Unit
loseElement loc = getDriver >>= flip S.loseElement loc >>> lift

-- | Tries to find child, if has no success throws an error
childExact ∷ Element -> Locator -> Selenium Element
childExact el loc = lift $ S.childExact el loc

startSpying ∷ Selenium Unit
startSpying = getDriver >>= XHR.startSpying >>> lift

stopSpying ∷ Selenium Unit
stopSpying = getDriver >>= XHR.stopSpying >>> lift

clearLog ∷ Selenium Unit
clearLog = getDriver >>= XHR.clearLog >>> lift

getXHRStats ∷ Selenium (List XHRStats)
getXHRStats = getDriver >>= XHR.getStats >>> map fromFoldable >>> lift


getWindowHandle ∷ Selenium WindowHandle
getWindowHandle = getDriver >>= S.getWindowHandle >>> lift

getAllWindowHandles ∷ Selenium (List WindowHandle)
getAllWindowHandles = getDriver >>= S.getAllWindowHandles >>> lift

switchTo ∷ WindowHandle -> Selenium Unit
switchTo w = getDriver >>= S.switchTo w >>> lift

closeWindow ∷ Selenium Unit
closeWindow = getDriver >>= S.close >>> lift
