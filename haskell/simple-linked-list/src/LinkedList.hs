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

data LinkedList a = Nil | Cons a (LinkedList a)

datum :: LinkedList a -> a
datum (Cons x xs) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x : xs) = Cons x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x xs = Cons x xs

next :: LinkedList a -> LinkedList a
next (Cons x xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList x = fromList $ reverse (toList x)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
