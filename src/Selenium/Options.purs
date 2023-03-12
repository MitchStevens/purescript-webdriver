module Selenium.Options where

import Prelude

import Data.Exists (Exists)

foreign import data Options' :: Row Type -> Type



type ChromeOptions = Options' ( headless :: Void, noSandbox :: Void )
type EdgeOptions = Options' ()
type FirefoxOptions = Options' ()
type IEOptions = Options' ()
type SafariOptions = Options' ()
type Options = Exists Options'

foreign import chromeOptions :: ChromeOptions 
foreign import edgeOptions :: EdgeOptions 
foreign import firefoxOptions :: FirefoxOptions 
foreign import ieOptions :: IEOptions 
foreign import safariOptions :: SafariOptions 

--TODO: do more later
foreign import setHeadless 
  :: forall r. Options' ( headless :: Void | r ) 
  -> Options' ( headless :: Void | r)

foreign import setNoSandbox
  :: forall r. Options' ( noSandbox :: Void | r ) 
  -> Options' ( noSandbox :: Void | r)

