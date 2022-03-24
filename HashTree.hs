module HashTree (leaf, twig, node, buildTree, treeHash, drawTree, Tree(..)) where
import Hashable32
import Utils

data Tree a = Leaf Hash a | Node Hash (Tree a) (Tree a) | Twig Hash (Tree a)

-- instance Hashable a => Hashable (Tree a) where
--     hash (Leaf x _) = x
--     hash (Node x _ _) = x

treeHash :: (Hashable a) => Tree a -> Hash
treeHash (Leaf x _) = hash x
treeHash (Node x _ _) = x
treeHash (Twig x _) = x


leaf :: (Hashable a) => a -> Tree a
leaf x = Leaf (hash x) x

twig :: (Hashable a) => Tree a -> Tree a
twig x = Twig (hash (t, t)) x
    where t = treeHash x

node :: (Hashable a) => Tree a -> Tree a -> Tree a
node l r = Node (hash (treeHash l, treeHash r)) l r

buildTree :: (Hashable a) => [a] -> Tree a

buildOneLevel :: (Hashable a) => [Tree a] -> [Tree a]
buildOneLevel [l, r] = [node l r]
buildOneLevel (l:r:tail) = node l r:buildOneLevel tail
buildOneLevel [t] = [twig t]
buildOneLevel [] = []

buildDepthTree :: (Hashable a) => [Tree a] -> Tree a

buildDepthTree [t] = t
buildDepthTree t = buildDepthTree $ buildOneLevel t


buildTree x = buildDepthTree (map leaf x)

drawTree :: (Show a) => Tree a -> String
drawTreeWithDepth :: (Show a) => Int -> Tree a -> String
drawTree = drawTreeWithDepth 0

drawTreeWithDepth n (Leaf h a) = replicate n ' ' ++ showHash h ++ " " ++ show a ++ "\n"
drawTreeWithDepth n (Twig h t) = replicate n ' ' ++ showHash h ++ " +\n" ++ drawTreeWithDepth (n+1) t
drawTreeWithDepth n (Node h l r) = replicate n ' ' ++ showHash h ++ " -\n" ++ drawTreeWithDepth (n+1) l ++ drawTreeWithDepth (n+1) r

-- >>> drawTree $ buildTree "fubar"
-- "0x2e1cc0e4 -\n 0xfbfe18ac -\n  0x6600a107 -\n   0x00000066 'f'\n   0x00000075 'u'\n  0x62009aa7 -\n   0x00000062 'b'\n   0x00000061 'a'\n 0xd11bea20 +\n  0x7200b3e8 +\n   0x00000072 'r'\n"
