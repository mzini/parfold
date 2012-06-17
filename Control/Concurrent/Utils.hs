--------------------------------------------------------------------------------
-- | 
-- Module      :  Control.Concurrent.Util
-- Copyright   :  (c) Martin Avanzini <martin.avanzini@uibk.ac.at>, 
--                Georg Moser <georg.moser@uibk.ac.at>, 
--                Andreas Schnabl <andreas.schnabl@uibk.ac.at>,
-- License     :  LGPL (see COPYING)
--
-- Maintainer  :  Martin Avanzini <martin.avanzini@uibk.ac.at>
-- Stability   :  stable
-- Portability :  unportable      
-- 
-- This module provides implements utilities for concurrent programming.
--------------------------------------------------------------------------------

module Control.Concurrent.Utils 
    ( spawn
      -- | The call 'spawn command args input' 
      -- spawns the unix command @command@ on arguments @args@ and input @input@.
      -- It returns the triple @(exit,out,err)@ where @exit@ is the 'ExitCode' 
      -- of the command, @out@ the standard and @err@ the error output
    , timedKill
      -- | 'timedKill t m' executes the io action 'm' in the current thread.
      -- It returns 'Just r' where 'r' is the result of the io action if 
      -- 'm' returns before 't' /microseconds/. If 'm' does not return in that
      -- time, the call 'timedKill t m' sends a 'C.ThreadKilled' to 'm' and 
      -- returns 'Nothing'. Note that if 'm' may catch the 'C.ThreadKilled', 
      -- and ignore the timeout 't' this way.
    , threadKilled
      -- | Returns 'Just ()' iff the given exception is a 'C.ThreadKilled' action      
    , solveList)
where
import Control.Concurrent (forkIO, forkOS, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Concurrent.PFold (pfoldA, Return (..))
import Control.Monad (foldM, when, liftM)
import Control.Monad.Trans (liftIO)
import System.Exit	( ExitCode(..) )
import System.IO
import System.IO.Error
import System.Process
import qualified Control.Exception as C

spawn :: String -> [String] -> String -> IO (Maybe (ExitCode, String, String))
spawn cmd args input = C.bracket createProc finalize run
    where createProc = do e <- createProcess (proc cmd args) { std_in  = CreatePipe,
                                                              std_out = CreatePipe,
                                                              std_err = CreatePipe }
                          case e of 
                            (Just inh, Just outh, Just errh, pid) 
                              -> return (inh, outh, errh, pid) 
                            _ -> error "Control.Concurrent.Utils.spawn could not create process"

          retrying d m = do s <- try $ m
                            case s of 
                              Left _ -> (hPutStr stderr msg) >> retrying d m
                                  where msg = "Control.Concurrent.Utils.spawn: Retrying " ++ d ++ "..."
                              Right a -> return a
          finalize (inh, outh, errh, pid) = 
              do _ <- retrying "terminate" $ terminateProcess pid
                 _ <- try $ waitForProcess pid
                 hClose inh >> hClose outh >> hClose errh
                                               
          run (inh, outh, errh, pid) = 
              do output <- hGetContents outh
                 errors <- hGetContents errh
                 outMVar <- newEmptyMVar
                 _ <- forkOS $ C.evaluate (length output) >> putMVar outMVar ()
                 when (not (null input)) $ do hPutStr inh input; hFlush inh;
                 hClose inh
                 takeMVar outMVar
                 finalize (inh, outh, errh, pid)
                 ex <- waitForProcess pid
                 return $ Just (ex, output, errors)

timedKill :: Int -> IO a -> IO (Maybe a)
timedKill n m = do pid <- myThreadId
                   C.handleJust threadKilled 
                    (const $ return Nothing)
                    (C.bracket 
                          (forkIO $ threadDelay n >> killThread pid)
                          killThread
                          (const $ Just `liftM` m))

threadKilled :: C.SomeException -> Maybe ()
threadKilled e = case C.fromException e of 
                   Just C.ThreadKilled -> Just ()
                   _ -> Nothing

solveList :: Bool -> [IO (Return p)] -> IO (Either [p] [p])
solveList par l = liftIO $ (if par then solvePar else solveSeq) l
    where solveSeq = foldM comb (Right [])
              where comb (Right ps) ms = do p' <- ms 
                                            case p' of 
                                              Stop     p -> return $ Left $ p:ps
                                              Continue p -> return $ Right $ p:ps
                    comb e          _  = return e

          solvePar = pfoldA comb (Right [])
              where comb (Right ps) (Continue p) = Continue $ Right $ p:ps 
                    comb (Right ps) (Stop p)     = Stop $ Left (p:ps)
                    comb _          _            = error "Processor: Captain, what happen?"