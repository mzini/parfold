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

spawn :: String -> [String] -> String -> IO (Maybe (ExitCode, String))
spawn cmd args input = 
    do (Just inh, Just outh, _, pid) <- createProcess (proc cmd args){ std_in  = CreatePipe,
                                                                      std_out = CreatePipe,
                                                                      std_err = Inherit }
       catchErr inh outh pid (spwn inh outh pid)
    where spwn inh outh pid = do output  <- hGetContents outh
                                 outMVar <- newEmptyMVar
                                 forkOS $ C.evaluate (length output) >> putMVar outMVar ()

                                 when (not (null input)) $ do hPutStr inh input; hFlush inh; 
                                 hClose inh
                                 takeMVar outMVar
                                 finalize inh outh
                                 ex <- waitForProcess pid
                                 return $ Just (ex, output)
--                                 case ex of
--                                   ExitSuccess   -> return $ Right output
--                                   ExitFailure r -> return $ Left $ "UnixSignal(" ++ show r ++ ")"

          finalize inh outh = hClose inh >> hClose outh

          catchErr inh outh pid ma = C.catchJust threadKilled ma handler
              where handler () = do terminateProcess pid
                                    waitForProcess pid
                                    finalize inh outh
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