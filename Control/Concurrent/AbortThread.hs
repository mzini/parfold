{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.AbortThread 
    ( abort
    , forkAbortableIO
    , forkAbortableOS
    , abortAfter
    , AbortMsg
    )
where 

import Data.Unique (Unique, newUnique)
--import Control.Monad (liftM)
--import System.Process (ProcessHandle)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent (ThreadId, forkIO, forkOS, myThreadId)
import qualified Control.Exception as C
import Data.Typeable 

data AbortThread = AbortThread deriving (Eq, Typeable)
newtype AbortMsg = AbortMsg ThreadId

instance Show AbortThread where show _ = "<<abortThread>>"
instance C.Exception AbortThread

catchAbort :: IO a -> IO a -> IO a
catchAbort m handler =  m `C.catch` \ AbortThread -> handler

abort :: MonadIO m => AbortMsg -> m ()
abort (AbortMsg pid) = liftIO $ C.throwTo pid AbortThread

abortMe :: MonadIO m => m ()
abortMe = liftIO $ do pid <- myThreadId
                      C.throwTo pid AbortThread


forkAbortable :: MonadIO m => (IO () -> IO ThreadId) -> IO () -> m AbortMsg
forkAbortable frk m = liftIO $ C.block $ do pid <- frk $ (C.unblock m) `catchAbort` return ()
                                            return $ AbortMsg pid

forkAbortableIO :: MonadIO m => IO () -> m AbortMsg
forkAbortableIO = forkAbortable forkIO

forkAbortableOS :: MonadIO m => IO () -> m AbortMsg
forkAbortableOS = forkAbortable forkOS

abortAfter :: MonadIO m => Int -> m a -> m (Maybe a)
abortAfter i m = undefined 
    -- where abortAfter' = do C.bracket 
    --                         (forkAbortableIO m)
    --                         abort
    --                         (\ msg -> undefined)


-- abortThread :: MonadIO m => ProcessHandle -> m ()
-- abortThread pid = throwTo 