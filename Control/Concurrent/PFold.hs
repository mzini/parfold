--------------------------------------------------------------------------------
-- | 
-- Module      :  Control.Concurrent.PFold
-- Copyright   :  (c) Martin Avanzini <martin.avanzini@uibk.ac.at>, 
--                Georg Moser <georg.moser@uibk.ac.at>, 
--                Andreas Schnabl <andreas.schnabl@uibk.ac.at>,
-- License     :  LGPL (see COPYING)
--
-- Maintainer  :  Martin Avanzini <martin.avanzini@uibk.ac.at>
-- Stability   :  stable
-- Portability :  unportable      
-- 
-- This module provides implements /parallel folding/ of associative 
-- operators, and some convenience utilities implemented on top of it.
--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.PFold 
 ( 
   pfold
   -- | The call 'pfold f a ms' executes the io actions ms in 
   -- parallel, and collects the results of using the function 
   -- @f@ and the initial value @a@. No guarantee on the order
   -- of the folding is given, and thus @f@ should be associative.
   --
   -- If an io action from 'ms' receives an uncought exception, 
   -- then the specific io action is silently ignored. 
 , pfoldA   
   -- | Same as 'pfold' but allows the folding operator to abort 
   -- the computation and return a result immediately. All pending
   -- spawned io actions are cilled.
 , Return (..)
   -- | The result type of the folding function for the 'pfoldA'.
   
   -- * Utilities Implemented on top of 'pfold'
   -- | The following actions are all implemented on top of 
   -- 'pfoldA', hence again actions that receive an uncaught 
   -- exception are silently ignored
 , fastest
   -- | The call 'fastest a ms' executes the io actions 'ms' in parallel, 
   -- and returns the first computed result, or 'a' if the io actions 'ms'
   -- are empty.
 , fastestSatisfying
   -- | Same as fastest, but additionally checks a predicate on the returned
   -- result. If the predicate does not hold, the result is discarded.
 , alt
   -- | The call 'alt m ms' executes all actions in 'm:ms' in parallel, 
   -- and returns the fastest computed result. 
 , choice
   -- | 'choice a b == alt a [b]'
 , par
   -- | 'par ma mb' runs the action 'ma' and 'mb' in parallel, returning their result.
   -- The actions 'ma' and 'mb' need to properly catch all exceptions, or otherwise
   -- the call might hang indefinately
 , (<+>)
   -- | Infix version of 'par'.
   
 , (<|>)
   -- | Infix version of 'choice'.
 )   
where
import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as C
import Control.Monad
import Control.Concurrent.Chan (readChan, writeChan, newChan)

data Return a = Stop a -- ^ Stop computation of other subprocesses
              | Continue a -- ^ Usual return behaviour

pfoldA :: (a -> b -> Return a) -> a -> [IO b] -> IO a
pfoldA f e ios = do mv <- newChan
                    C.bracket
                         (mapM (spwn mv) ios) -- spawned children are blocked from asynchrounous exceptions, only the given io from ios is unblocked per child
                         killAll
                         (collect mv e)
    where spwn mv io = forkIO $ do res <- Just `liftM` evalIO `C.catch` (\ (_ :: C.SomeException) -> return Nothing)
                                   writeChan mv res
              where evalIO = C.unblock $ io >>= C.evaluate

          collect _  a  []       = return a
          collect mv a  (_:tids) = handleKilled a m
            where m = do 
                    res <- readChan mv
                    case res of 
                      Just b  -> 
                        case f a b of 
                          Stop r     -> return r
                          Continue r -> collect mv r tids
                      Nothing -> collect mv a tids

          killAll = mapM killThread

          handleKilled a = C.handleJust threadKilled (const $ return a)
          threadKilled :: C.SomeException -> Maybe ()
          threadKilled er = 
            case C.fromException er of 
              Just C.ThreadKilled -> Just ()
              _ -> Nothing

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
          plus _                       _         = Stop $ error "unexpected arguments to Control.Concurrent.PFold.par, subfunction plus"
  
alt :: IO a -> [IO a] -> IO a
alt m ms = do Just r <- pfoldA plus Nothing (m:ms)
              return r
    where plus Nothing  b = Stop $ Just b
          plus (Just b) _ = Stop $ Just b

choice :: IO a -> IO a -> IO a
choice a b = alt a [b]

(<+>) :: forall a b. IO a -> IO b -> IO (a, b)
(<+>) = par

(<|>) :: forall a. IO a -> IO a -> IO a
(<|>) = choice
