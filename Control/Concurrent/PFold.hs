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
 , choice
 , par
 , alt
 , (<+>)
 , (<|>))
where
import Control.Concurrent (forkIO, killThread)
import Control.Parallel (pseq)
import qualified Control.Exception as C
import Control.Monad
import System.IO
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

data Return a = Stop a
              | Continue a

pfoldA :: (a -> b -> Return a) -> a -> [IO b] -> IO a
pfoldA f e ios = do mv <- newEmptyMVar 
                    C.bracket
                         (mapM (spwn mv) ios)
                         killAll
                         (collect mv e)
                 
    where spwn mv io = forkIO $ do b <- io
                                   pseq b (return ())
                                   putMVar mv b

          collect _  a  []       = return a
          collect mv a  (_:tids) = 
              do b <- takeMVar mv
                 case f a b of 
                   Stop r     -> return r
                   Continue r -> collect mv r tids

          killAll = mapM killThread

pfold :: (a -> b -> a) -> a -> [IO b] -> IO a
pfold f = pfoldA $ \ a b -> Continue (f a b)

fastest :: a -> [IO a] -> IO a
fastest = fastestSatisfying $ const True

fastestSatisfying :: (a -> Bool) -> a -> [IO a] -> IO a
fastestSatisfying f = pfoldA (\ a b -> if f b then Stop b else Continue a)


par :: IO a -> IO b -> IO (a, b)
par ma mb = do Just (Right r) <- pfoldA plus Nothing [ Left `liftM` ma
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

(<+>) = par
(<|>) = choice
