module Control.Concurrent.Utils 
    ( spawn
    , threadKilled
    , timedKill)
where
import System.Process
import Control.Concurrent (forkIO, forkOS, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import System.IO
import System.IO.Error
import Data.Typeable
import System.Exit	( ExitCode(..) )
import Control.Monad (when)
import qualified Control.Exception as C

spawn :: String -> [String] -> String -> IO (Maybe (ExitCode, String, String))
spawn cmd args input = 
    do (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args){ std_in  = CreatePipe,
                                                                               std_out = CreatePipe,
                                                                               std_err = CreatePipe }
       catchErr inh outh errh pid (spwn inh outh errh pid)
    where spwn inh outh errh pid = do output <- hGetContents outh
                                      errors <- hGetContents errh
                                      outMVar <- newEmptyMVar
                                      forkOS $ C.evaluate (length output) >> putMVar outMVar ()
                                      when (not (null input)) $ do hPutStr inh input; hFlush inh;
                                      hClose inh
                                      takeMVar outMVar
                                      finalize inh outh errh
                                      ex <- waitForProcess pid
                                      return $ Just (ex, output, errors)
--                                     case ex of
--                                       ExitSuccess   -> return $ Right output
--                                       ExitFailure r -> return $ Left $ "UnixSignal(" ++ show r ++ ")"

          finalize inh outh errh = hClose inh >> hClose outh >> hClose errh

          catchErr inh outh errh pid ma = C.catchJust threadKilled ma handler
              where handler () = do terminateProcess pid
                                    waitForProcess pid
                                    finalize inh outh errh
                                    return Nothing

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