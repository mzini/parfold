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
    , solveList)
where
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Concurrent.PFold (pfoldA, Return (..))
import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import System.Exit	( ExitCode(..) )
import System.IO
import System.IO.Error (tryIOError)
import System.Process
import qualified Control.Exception as C

spawn :: String -> [String] -> String -> IO (Maybe (ExitCode, String, String))
spawn cmd args input = C.bracket createProc finalize run
    where createProc = do 
            e <- createProcess (proc cmd args) { std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = CreatePipe }
            case e of 
              (Just inh, Just outh, Just errh, pid) 
                -> return (inh, outh, errh, pid) 
              _ -> error "Control.Concurrent.Utils.spawn could not create process"

          finalize (inh, outh, errh, pid) = do 
            _ <- tryIOError $ terminateProcess pid
            _ <- tryIOError $ waitForProcess pid
            _ <- tryIOError $ hClose inh -- catch EPIPE errors (hClose implies hFlush)
            hClose outh >> hClose errh
                                               
          run (inh, outh, errh, pid) = do 
            output <- hGetContents outh
            errors <- hGetContents errh
            outMVar <- newEmptyMVar
            _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()
            hPutStr inh input
            hClose inh
            takeMVar outMVar
            ex <- waitForProcess pid
            return $ Just (ex, output, errors)

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