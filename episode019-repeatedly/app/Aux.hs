module Aux where

import Control.Monad
import Data.Functor.Identity

{-------------------------------------------------------------------------------
  Examples

  These two examples illustrate two different ways in which we might be lazy:
  in the /values/ (example1) or in the /actions/ (example2).
-------------------------------------------------------------------------------}

example1 :: Monad m => m [Int]
example1 = do
    x <- return undefined
    y <- return 1
    return [y, x]

example2 :: Monad m => m [Int]
example2 = do
    x <- undefined
    y <- return 1
    return [y, x]

{-------------------------------------------------------------------------------
  Prelim: regular Identity monad is lazy
-------------------------------------------------------------------------------}

runIdentity1 :: [Int]
runIdentity1 = take 1 $ runIdentity example1

runIdentity2 :: [Int]
runIdentity2 = take 1 $ runIdentity example2

{-------------------------------------------------------------------------------
  Strict identity monad
-------------------------------------------------------------------------------}

newtype StrictId a = StrictId { runStrictId :: a }

instance Functor StrictId where
  fmap = liftM

instance Applicative StrictId where
  pure !x = StrictId x
  (<*>)   = ap

instance Monad StrictId where
  StrictId (!x) >>= k = k x

runStrictId1 :: [Int]
runStrictId1 = take 1 $ runStrictId example1

runStrictId2 :: [Int]
runStrictId2 = take 1 $ runStrictId example2

{-------------------------------------------------------------------------------
  Lazy identity monad

  'Identity' is lazy in its argument (since it's a @newtype@). We can define a
  variant (using @data@) which is lazy in its argument. Interestingly, this
  monad sits somewhere in between 'Identity' and 'StrictId':

  >         | Identity | StrictId | LazyId
  > values  | lazy     | strict   | lazy
  > actions | lazy     | strict   | strict
-------------------------------------------------------------------------------}

data LazyId a = LazyId { runLazyId :: a }

instance Functor LazyId where
  fmap = liftM

instance Applicative LazyId where
  pure  = LazyId
  (<*>) = ap

instance Monad LazyId where
  LazyId x >>= k = k x

runLazyId1 :: [Int]
runLazyId1 = take 1 $ runLazyId example1

runLazyId2 :: [Int]
runLazyId2 = take 1 $ runLazyId example2

{-------------------------------------------------------------------------------
  Monad transformer that makes any monad strict
-------------------------------------------------------------------------------}

newtype MkStrict m a = MkStrict { runMkStrict :: m a }

instance Monad m => Functor (MkStrict m) where
  fmap = liftM

instance Monad m => Applicative (MkStrict m) where
  pure !x = MkStrict (pure x)
  (<*>)   = ap

instance Monad m => Monad (MkStrict m) where
  MkStrict ma >>= k = MkStrict $ ma >>= \(!a) -> runMkStrict (k a)

runMkStrictIdentity1 :: [Int]
runMkStrictIdentity1 = take 1 $ runIdentity . runMkStrict $ example1

runMkStrictIdentity2 :: [Int]
runMkStrictIdentity2 = take 1 $ runIdentity . runMkStrict $ example2

runMkStrictLazyId1 :: [Int]
runMkStrictLazyId1 = take 1 $ runLazyId . runMkStrict $ example1

runMkStrictLazyId2 :: [Int]
runMkStrictLazyId2 = take 1 $ runLazyId . runMkStrict $ example2

{-------------------------------------------------------------------------------
  Monad transformer that makes any monad lazy
-------------------------------------------------------------------------------}

data Box a = Box { unbox :: a }

newtype MkLazy m a = MkLazy { runMkLazy :: m (Box a) }

instance Monad m => Functor (MkLazy m) where
  fmap = liftM

instance Monad m => Applicative (MkLazy m) where
  pure x = MkLazy (pure $ Box x)
  (<*>)  = ap

instance Monad m => Monad (MkLazy m) where
  (MkLazy x) >>= f = MkLazy $ x >>= \(Box a) -> runMkLazy (f a)

runMkLazyStrictId1 :: [Int]
runMkLazyStrictId1 = take 1 $ unbox . runStrictId . runMkLazy $ example1

runMkLazyStrictId2 :: [Int]
runMkLazyStrictId2 = take 1 $ unbox . runStrictId . runMkLazy $ example2
