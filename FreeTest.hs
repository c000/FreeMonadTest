{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Writer
import Control.Monad.Trans

data SomeData a b = SomeData a b
    deriving (Functor, Show)

data SingleData a = SingleData a
    deriving (Functor, Show)

-- instance Functor (SomeData a) where
--     fmap f (SomeData a b) = SomeData a (f b)

type MyFree a = Free (SomeData a)

someData x = liftF $ SomeData x ()

myData :: WriterT String (MyFree String) ()
myData = do
    tell "aaaa\n"
    lift $ someData "piyo"
    lift $ return ()

main = do
    print $ runWriterT myData
