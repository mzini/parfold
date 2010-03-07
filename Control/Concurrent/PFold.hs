module Control.Concurrent.PFold 
 -- ( Return (..)
 -- , pfoldA
 -- , pfold
 -- , fastest
 -- , fastestSatisfying
 -- , choice
 -- )
where
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Parallel (pseq)
import System.Process
import System.Exit	( ExitCode(..) )
import System.IO.Error
import qualified Control.Exception as C
import Control.Monad
import System.IO
import Data.Typeable
import System.Timeout
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

data Return a = Stop a
              | Continue a

pfoldA :: (a -> b -> Return a) -> a -> [IO b] -> IO a
pfoldA f e ios = do mv <- newEmptyMVar 
                    tids <- mapM (spwn mv) ios
                    (collect mv tids e `C.finally` killAll tids)
                 
    where spwn mv io = forkIO $ do b <- io
                                   pseq b (return ())
                                   putMVar mv b

          collect _  []       a = return a
          collect mv (_:tids) a = 
              do b <- takeMVar mv
                 case f a b of 
                   Stop r     -> return r
                   Continue r -> collect mv tids r

          killAll = mapM killThread

pfold :: (a -> b -> a) -> a -> [IO b] -> IO a
pfold f = pfoldA $ \ a b -> Continue (f a b)

fastest :: a -> [IO a] -> IO a
fastest = fastestSatisfying $ const True

fastestSatisfying :: (a -> Bool) -> a -> [IO a] -> IO a
fastestSatisfying f = pfoldA (\ a b -> if f b then Stop b else Continue a)


runpar :: IO a -> IO b -> IO (a, b)
runpar ma mb = do Just (Right r) <- pfoldA plus Nothing [ Left `liftM` ma
                                                       , Right `liftM` mb]
                  return r
    where plus Nothing                 b         = Continue $ Just $ Left b
          plus (Just (Left (Left a)))  (Right b) = Stop $ Just $ Right (a, b)
          plus (Just (Left (Right b))) (Left a)  = Stop $ Just $ Right (a, b)
  
alt :: IO a -> [IO a] -> IO a
alt m ms = do Just r <- pfoldA plus Nothing (m:ms)
              return r
    where plus Nothing  b = Stop $ Just b
          plus (Just b) _ = Stop $ Just b

choice :: IO a -> IO a -> IO a
choice a b = alt a [b]

(<+>) = runpar
(<|>) = choice


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
                                    putStrLn "handler called"
                                    return $ Nothing

-- ls :: String -> IO (Maybe [String])
-- ls str = (liftM lines) `liftM` spawn "ls" [str] ""

-- find :: String -> IO (Maybe [String])
-- find str = (liftM lines) `liftM` spawn "find" [str] ""


data Timeout = Timeout deriving (Show, Typeable)
-- instance C.Exception Timeout 

timedKill :: Int -> IO a -> IO (Maybe a)
timedKill n m = do pid <- myThreadId
                   C.handleJust 
                    threadKilled 
                    (const $ return Nothing)
                    (C.bracket 
                          (forkIO $ threadDelay n >> killThread pid)
                          killThread
                          (const $ C.unblock m >>= return . Just))

threadKilled :: C.Exception -> Maybe ()
threadKilled e = case cast e of 
                   Just C.ThreadKilled -> Just ()
                   _ -> Nothing



fib 0 = return 0
fib 1 = return 1
fib 2 = return 3
fib n = do a <- fib (n - 1)
           b <- fib (n - 2)
           return $ a + b