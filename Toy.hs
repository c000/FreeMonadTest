import Control.Monad.Trans
import Control.Monad.Free
import Control.Monad.Writer
import Data.Monoid

data Toy b next = Output b next
                | Bell     next
                | Done
    deriving (Show)

instance Functor (Toy b) where
    fmap f (Output b next) = Output b (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f (Done         ) = Done

type ToyF b = Free (Toy b)

output :: b -> ToyF b ()
output x = liftF (Output x ())
bell :: ToyF b ()
bell = liftF (Bell ())
done :: ToyF b a
done = liftF (Done)

program :: ToyF Char ()
program = do
    output 'A'
    bell
    done

prog2 :: WriterT String (ToyF Char) ()
prog2 = do
    lift $ output 'A'
    tell "hoge"
    tell "piyo"
    lift $ output 'B'
    tell "fuga"
    lift $ bell

main = do
    print $ runWriterT prog2
    print $ execWriterT prog2
