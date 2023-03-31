{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

module Keter.Context where

import           Keter.Common
import           Control.Monad.Trans       (lift)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.IO.Unlift   (MonadUnliftIO)
import           Control.Monad.Logger      (MonadLogger, MonadLoggerIO, LoggingT(..), runLoggingT)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT, ask
                                           , withReaderT)

-- | The top-level keter context monad, carrying around the main logger and some locally relevant configuration structure.
--
-- See this blog post for an explanation of the design philosophy: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
--
-- TODO: generalize as /contexts/ instead of /configs/? Since not every state being passed
-- around can be intuitively thought of as a config per se. Ex. AppManager
newtype KeterM cfg a = KeterM { runKeterM :: LoggingT (ReaderT cfg IO) a }
  deriving newtype (Functor, Applicative, Monad,
                    MonadUnliftIO,
                    MonadIO, MonadLogger, MonadLoggerIO,
                    MonadReader cfg)

withMappedConfig :: (cfg -> cfg') -> KeterM cfg' a -> KeterM cfg a
withMappedConfig f (KeterM ctx) = 
    KeterM $ LoggingT $ \logger -> withReaderT f $ runLoggingT ctx logger
