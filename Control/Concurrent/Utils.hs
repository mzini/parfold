module Control.Concurrent.Utils 
    ( spawn
    , threadKilled
    , timedKill)
where
import System.Process
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import System.IO
import System.IO.Error
import Data.Typeable
import System.Exit	( ExitCode(..) )
import Control.Monad (when)
import qualified Control.Exception as C

spawn :: String -> [String] -> String -> IO (Maybe String)
spawn cmd args input = do (Just inh, Just outh, _, pid) <- createProcess (proc cmd args){ std_in  = CreatePipe,
                                                                                         std_out = CreatePipe,
                                                                                         std_err = Inherit }
                          catchErr inh outh pid (spwn inh outh pid)
    where spwn inh outh pid = do output  <- hGetContents outh
                                 outMVar <- newEmptyMVar
                                 forkIO $ C.evaluate (length output) >> putMVar outMVar ()

                                 when (not (null input)) $ do hPutStr inh input; hFlush inh
                                 takeMVar outMVar
                                 finalize inh outh
                                 ex <- waitForProcess pid
                                 case ex of
                                   ExitSuccess   -> return $ Just output
                                   ExitFailure r -> return Nothing

          finalize inh outh = hClose inh >> hClose outh
          
          catchErr inh outh pid ma = C.catchJust threadKilled ma handler
              where handler () = do terminateProcess pid
                                    waitForProcess pid
                                    finalize inh outh
                                    return $ Nothing

timedKill :: Int -> IO a -> IO (Maybe a)
timedKill n m = do pid <- myThreadId
                   C.handleJust threadKilled 
                    (const $ return Nothing)
                    (C.bracket 
                          (forkIO $ threadDelay n >> killThread pid)
                          killThread
                          (const $ C.unblock m >>= return . Just))

threadKilled e = case C.fromException e of 
                   Just C.ThreadKilled -> Just ()
                   _ -> Nothing