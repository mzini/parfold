module Control.Concurrent.PFold 
 ( Return (..)
 , pfoldA
 , pfold
 , fastest
 , fastestSatisfying
 )
where
import Control.Concurrent (forkIO, killThread)
import Control.Parallel (pseq)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

data Return a = Stop a
              | Continue a

pfoldA :: (a -> b -> Return a) -> a -> [IO b] -> IO a
pfoldA f e ios = do
  mv <- newEmptyMVar 
  tids <- mapM (spwn mv) ios
  r <- collect mv tids e
  killAll tids
  return r
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

