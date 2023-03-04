-- | Most functions of `Selenium` use `Driver` as an argument
-- | This module supposed to make code a bit cleaner through
-- | putting `Driver` to `ReaderT`
module Selenium.Monad
  where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..))
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

-- TODO: add ExceptT to Selenium monad
data WebDriverException
  = ElementNotSelectableException
  | ElementNotInteractableException
  | ElementNotVisibleException
  | NoSuchElementException
  | NoSuchFrameException
  | NoAlertPresentException
  | NoSuchWindowException
  | StaleElementReferenceException
  | SessionNotFoundException
  | TimeoutException
  | WebDriverException

-- | `Driver` is field of `ReaderT` context
-- | Usually selenium tests are run with tons of configs (i.e. xpath locators,
-- | timeouts) all those configs can be putted to `Selenium e o a`
type Selenium o a =
  ReaderT { driver ∷ Driver, defaultTimeout ∷ Milliseconds |o}
    ---(ExceptT WebDriverException A.Aff) a
    Aff a

-- | get driver from context
getDriver ∷ ∀ o. Selenium o Driver
getDriver = _.driver <$> ask

getWindow ∷ ∀ o. Selenium o Window
getWindow =  getDriver >>= S.getWindow >>> lift

getWindowPosition ∷ ∀ o. Selenium o Location
getWindowPosition = getWindow >>= S.getWindowPosition >>> lift

getWindowSize ∷ ∀ o. Selenium o Size
getWindowSize = getWindow >>= S.getWindowSize >>> lift

maximizeWindow ∷ ∀ o. Selenium o Unit
maximizeWindow = getWindow >>= lift <<< S.maximizeWindow

setWindowPosition ∷ ∀ o. Location -> Selenium o Unit
setWindowPosition pos = getWindow >>= S.setWindowPosition pos >>> lift

setWindowSize ∷ ∀ o. Size -> Selenium o Unit
setWindowSize size = getWindow >>= S.setWindowSize size >>> lift

getWindowScroll ∷ ∀ o. Selenium o Location
getWindowScroll = getDriver >>= S.getWindowScroll >>> lift

-- LIFT `Aff` combinators to `Selenium.Monad`
apathize ∷ ∀ o a. Selenium o a -> Selenium o Unit
apathize check = ReaderT \r ->
  A.apathize $ runReaderT check r

attempt ∷ ∀ o a. Selenium o a -> Selenium o (Either Error a)
attempt check = ReaderT \r ->
  A.attempt $ runReaderT check r

later ∷ ∀ d o a. Duration d ⇒ d -> Selenium o a -> Selenium o a
later time check = do
  lift (A.delay (fromDuration time))
  check

-- LIFT `Selenium` funcs to `Selenium.Monad`
get ∷ ∀ o. String -> Selenium o Unit
get url =
  getDriver >>= lift <<< flip S.get url

wait ∷ ∀ d o. Duration d ⇒ Selenium o Boolean -> d -> Selenium o Unit
wait check time = ReaderT \r ->
  S.wait (runReaderT check r) (fromDuration time) r.driver

-- | Tries the provided Selenium computation repeatedly until the provided timeout expires
tryRepeatedlyTo' ∷ ∀ d a o. Duration d ⇒ d -> Selenium o a -> Selenium o a
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
tryRepeatedlyTo ∷ ∀ a o. Selenium o a -> Selenium o a
tryRepeatedlyTo selenium = ask >>= \r ->
  tryRepeatedlyTo' r.defaultTimeout selenium

byCss ∷ ∀ o. String -> Selenium o Locator
byCss = lift <<< S.byCss

byXPath ∷ ∀ o. String -> Selenium o Locator
byXPath = lift <<< S.byXPath

byId ∷ ∀ o. String -> Selenium o Locator
byId = lift <<< S.byId

byName ∷ ∀ o. String -> Selenium o Locator
byName = lift <<< S.byName

byClassName ∷ ∀ o. String -> Selenium o Locator
byClassName = lift <<< S.byClassName

byDataAutomation :: forall o. String -> Selenium o Locator
byDataAutomation dataAutomationTag =
  byXPath ("//*[@data-automation='" <> dataAutomationTag <>"']")

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
--locator ∷ ∀ o. (Element -> Selenium o Element) -> Selenium o Locator
--locator checkFn = ReaderT \r ->
--
--  --S.affLocator  :: (Element -> Aff Element) -> Aff Locator
--
--  ReaderT \r ->
--  S.affLocator (\el -> runReaderT (checkFn el) r)

-- | Tries to find element and return it wrapped in `Just`
findElement ∷ ∀ o. Locator -> Selenium o (Maybe Element)
findElement l = do
  getDriver >>= lift <<< flip S.findElement l

findElements ∷ ∀ o. Locator -> Selenium o (List Element)
findElements l =
  getDriver >>= lift <<< flip S.findElements l

-- | Tries to find child and return it wrapped in `Just`
findChild ∷ ∀ o. Element -> Locator -> Selenium o (Maybe Element)
findChild el loc = lift $ S.findChild el loc

findChildren ∷ ∀ o. Element -> Locator -> Selenium o (List Element)
findChildren el loc = lift $ S.findChildren el loc

getInnerHtml ∷ ∀ o. Element -> Selenium o String
getInnerHtml = lift <<< S.getInnerHtml

getSize ∷ ∀ o. Element -> Selenium o Size
getSize = lift <<< S.getSize

getLocation ∷ ∀ o. Element -> Selenium o Location
getLocation = lift <<< S.getLocation

isDisplayed ∷ ∀ o. Element -> Selenium o Boolean
isDisplayed = lift <<< S.isDisplayed

isEnabled ∷ ∀ o. Element -> Selenium o Boolean
isEnabled = S.isEnabled >>> lift

getCssValue ∷ ∀ o. String -> Element -> Selenium o String
getCssValue key el = lift $ S.getCssValue el key

getAttribute ∷ ∀ o. String -> Element -> Selenium o (Maybe String)
getAttribute attr el = lift $ S.getAttribute el attr

getText ∷ ∀ o. Element -> Selenium o String
getText el = lift $ S.getText el

clearEl ∷ ∀ o. Element -> Selenium o Unit
clearEl = lift <<< S.clearEl

clickEl ∷ ∀ o. Element -> Selenium o Unit
clickEl = lift <<< S.clickEl

sendKeysEl ∷ ∀ o. String -> Element -> Selenium o Unit
sendKeysEl ks el = lift $ S.sendKeysEl ks el

script ∷ ∀ o. String -> Selenium o Foreign
script str =
  getDriver >>= flip S.executeStr str >>> lift

getCurrentUrl ∷ ∀ o. Selenium o String
getCurrentUrl = getDriver >>= S.getCurrentUrl >>> lift

navigateBack ∷ ∀ o. Selenium o Unit
navigateBack = getDriver >>= S.navigateBack >>> lift

navigateForward ∷ ∀ o. Selenium o Unit
navigateForward = getDriver >>= S.navigateForward >>> lift

navigateTo ∷ ∀ o. String -> Selenium o Unit
navigateTo url = getDriver >>= S.navigateTo url >>> lift

setFileDetector ∷ ∀ o. FileDetector -> Selenium o Unit
setFileDetector fd = getDriver >>= flip S.setFileDetector fd >>> lift

getTitle ∷ ∀ o. Selenium o String
getTitle = getDriver >>= S.getTitle >>> lift


-- | Run sequence of actions
sequence ∷ ∀ o. AS.Sequence Unit -> Selenium o Unit
sequence seq = do
  getDriver >>= lift <<< flip AS.sequence seq

-- | Same as `sequence` but takes function of `ReaderT` as an argument
actions
  ∷ ∀ o
  . ({ driver ∷ Driver, defaultTimeout ∷ Milliseconds | o } -> AS.Sequence Unit)
  -> Selenium o Unit
actions seqFn = do
  ctx ← ask
  sequence $ seqFn ctx

-- | Stop computations
stop ∷ ∀ o. Selenium o Unit
stop = wait (later msMax $ pure false) msMax
  where
  msMax = Milliseconds (Int.toNumber (top ∷ Int))

refresh ∷ ∀ o. Selenium o Unit
refresh = getDriver >>= S.refresh >>> lift

quit ∷ ∀ o. Selenium o Unit
quit = getDriver >>= S.quit >>> lift

takeScreenshot ∷ ∀ o. Selenium o String
takeScreenshot = getDriver >>= S.takeScreenshot >>> lift

saveScreenshot ∷ ∀ o. String -> Selenium o Unit
saveScreenshot name = getDriver >>= S.saveScreenshot name >>> lift

-- | Tries to find element, if has no success throws an error
findExact ∷ ∀ o. Locator -> Selenium o Element
findExact loc = getDriver >>= flip S.findExact loc >>> lift

-- | Tries to find element and throws an error if it succeeds.
loseElement ∷ ∀ o. Locator -> Selenium o Unit
loseElement loc = getDriver >>= flip S.loseElement loc >>> lift

-- | Tries to find child, if has no success throws an error
childExact ∷ ∀ o. Element -> Locator -> Selenium o Element
childExact el loc = lift $ S.childExact el loc

startSpying ∷ ∀ o. Selenium o Unit
startSpying = getDriver >>= XHR.startSpying >>> lift

stopSpying ∷ ∀ o. Selenium o Unit
stopSpying = getDriver >>= XHR.stopSpying >>> lift

clearLog ∷ ∀ o. Selenium o Unit
clearLog = getDriver >>= XHR.clearLog >>> lift

getXHRStats ∷ ∀ o. Selenium o (List XHRStats)
getXHRStats = getDriver >>= XHR.getStats >>> map fromFoldable >>> lift


getWindowHandle ∷ ∀ o. Selenium o WindowHandle
getWindowHandle = getDriver >>= S.getWindowHandle >>> lift

getAllWindowHandles ∷ ∀ o. Selenium o (List WindowHandle)
getAllWindowHandles = getDriver >>= S.getAllWindowHandles >>> lift

switchTo ∷ ∀ o. WindowHandle -> Selenium o Unit
switchTo w = getDriver >>= S.switchTo w >>> lift

closeWindow ∷ ∀ o. Selenium o Unit
closeWindow = getDriver >>= S.close >>> lift
