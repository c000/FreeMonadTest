{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free
import Control.Monad.Writer

data ListF a next = Nil | Node a (ListF a next)
    deriving (Show)

instance Functor (ListF a) where
    fmap f (Nil) = Nil
    fmap f (Node x xs) = Node x (fmap f xs)

type List a = Free (ListF a)

empty :: List a ()
empty = liftF Nil

singleton :: a -> List a ()
singleton x = liftF $ Node x Nil

cons :: a -> List a b -> List a b
cons x (Free ys) = liftF (Node x ys)
cons x (Pure y)  = cons x (singleton y)

myList :: List Int ()
myList = do
    singleton 1
    singleton 2

-- writerList :: WriterT String List ()
-- writerList = do
--     lift $ cons 1 $ cons 2 $ empty

main = do
    print $ myList
