{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.LogFile
    ( LogFile
    , start
    , addChunk
    , close
    ) where

import Keter.Prelude hiding (getCurrentTime)
import qualified Data.ByteString as S
import Data.Time (getCurrentTime)
import qualified System.IO as SIO
import qualified Filesystem as F
import qualified Data.Text as T

data Command = AddChunk S.ByteString
             | Close

newtype LogFile = LogFile (Command -> KIO ())

addChunk :: LogFile -> S.ByteString -> KIO ()
addChunk (LogFile f) bs = f $ AddChunk bs

close :: LogFile -> KIO ()
close (LogFile f) = f Close

start :: FilePath -- ^ folder to contain logs
      -> KIO (Either SomeException LogFile)
start dir = do
    res <- liftIO $ do
        createTree dir
        moveCurrent Nothing
    case res of
        Left e -> return $ Left e
        Right handle -> do
            chan <- newChan
            forkKIO $ loop chan handle 0
            return $ Right $ LogFile $ writeChan chan
  where
    current = dir </> "current.log"
    moveCurrent mhandle = do
        maybe (return ()) SIO.hClose mhandle
        x <- isFile current
        when x $ do
            now <- getCurrentTime
            rename current $ dir </> suffix now
        F.openFile current F.WriteMode
    suffix now = fromText (T.replace " " "_" $ T.takeWhile (/= '.') $ show now) <.> "log"
    loop chan handle total = do
        c <- readChan chan
        case c of
            AddChunk bs -> do
                let total' = total + S.length bs
                res <- liftIO $ S.hPut handle bs >> SIO.hFlush handle
                either $logEx return res
                if total' > maxTotal
                    then do
                        res2 <- liftIO $ moveCurrent $ Just handle
                        case res2 of
                            Left e -> do
                                $logEx e
                                deadLoop chan
                            Right handle' -> loop chan handle' 0
                    else loop chan handle total'
            Close ->
                liftIO (SIO.hClose handle) >>=
                    either $logEx return
    deadLoop chan = do
        c <- readChan chan
        case c of
            AddChunk _ -> deadLoop chan
            Close -> return ()

    maxTotal = 5 * 1024 * 1024 -- 5 MB
