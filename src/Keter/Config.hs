-- | This does a merger of V04 and V10
--   Effectivelly this provides the 'latest' config.
module Keter.Config
    ( module X
    ) where

import Keter.Config.V04 as X (PortSettings(..), TLSConfig(..))
import Keter.Config.V10 as X
       ( AppInput(..)
       , BackgroundConfig(..)
       , BundleConfig(..)
       , KeterConfig(..)
       , ListeningPort(..)
       , ProxyAction
       , ProxyActionRaw(..)
       , RedirectAction(..)
       , RedirectConfig(..)
       , RedirectDest(..)
       , RequiresSecure
       , RestartCount(..)
       , SourcePath(..)
       , Stanza(..)
       , StanzaRaw(..)
       , StaticFilesConfig(..)
       , WebAppConfig(..)
       )
