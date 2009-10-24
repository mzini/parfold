{-
This file is part of the Haskell Parfold Library.

The Haskell Parfold Library is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The Haskell Parfold Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the Haskell Parfold Library.  If not, see <http://www.gnu.org/licenses/>.
-}

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

