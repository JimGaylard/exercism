module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

-- The task is to create the data type `LinkedList`
-- and implement the functions below.

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Show)

instance Foldable LinkedList where
  foldr _ z Nil = z
  foldr f b (Cons x xs) = f x (foldr f b xs)

datum :: LinkedList a -> a
datum Nil = undefined
datum (Cons x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next Nil = Nil
next (Cons _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip Cons) Nil

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
