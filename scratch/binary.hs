module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Ord, Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b <  a = Node (insert' b left) a right
  | b >  a = Node left a (insert' b right)
insert' _ node = node

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable BinaryTree where
  foldr f acc node = foldr f acc (inorder node)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder


unfoldr :: (b -> Maybe (a, b)) -> b -> BinaryTree a
unfoldr f b0 = go b0
  where go b = case f b of
               Just (a, new_b) -> Node (go new_b) a (go new_b)
               Nothing         -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldr (\b -> if b == n then Nothing else Just (b, b+1)) 0

