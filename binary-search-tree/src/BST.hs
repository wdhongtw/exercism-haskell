module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Node a (BST a) (BST a) | Leaf deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf =  Nothing
bstLeft (Node _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf =  Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Node value _ _) = Just value

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList xs = foldl (myFlip insert) empty xs

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = (\b -> (\a -> f a b))

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node value left right)
    | x <= value = Node value (insert x left) right
    | otherwise = Node value left (insert x right)

singleton :: a -> BST a
singleton x = Node x empty empty

toList :: BST a -> [a]
toList Leaf = []
toList (Node value left right) = toList left ++ [value] ++ toList right
