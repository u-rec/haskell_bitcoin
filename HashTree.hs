module HashTree (leaf, 
    twig, 
    node, 
    buildTree, 
    treeHash, 
    drawTree, 
    buildProof, 
    showMerklePath, 
    merklePaths,
    verifyProof, 
    Tree(..), 
    MerkleProof(..)) where
import Hashable32
import Utils
import Distribution.Parsec.Newtypes (Set')

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

-- >>> drawTree $ buildTree "bitcoin"
-- "0x9989519e -\n 0x69f4387c -\n  0x62009aaf -\n   0x00000062 'b'\n   0x00000069 'i'\n  0x7400b6ff -\n   0x00000074 't'\n   0x00000063 'c'\n 0x5214666a -\n  0x6f00af26 -\n   0x0000006f 'o'\n   0x00000069 'i'\n  0x6e00ad98 +\n   0x0000006e 'n'\n"

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

getPath :: MerkleProof a -> MerklePath
getPath (MerkleProof _ path) = path

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
merklePaths :: Hashable a => a -> Tree a -> [MerklePath]

buildProof x (Leaf h y) = if hash x == h then Just $ MerkleProof x [] else Nothing
buildProof x (Twig _ t) = case proof of
    Just (MerkleProof d path) -> Just $ MerkleProof d (Right (treeHash t):path)
    Nothing -> Nothing
    where proof = buildProof x t

buildProof x (Node h l r) = case proofLeft of
    Just (MerkleProof d path) -> Just $ MerkleProof d $ Right (treeHash r):path
    Nothing -> case proofRight of
        Just (MerkleProof d path) -> Just $ MerkleProof d $ Left (treeHash l):path
        Nothing -> Nothing
    where
        proofLeft = buildProof x l
        proofRight = buildProof x r

merklePaths x (Leaf h y) = if hash x == h then [[]::MerklePath] else []::[MerklePath]
merklePaths x (Twig h t) = map (Right (treeHash t):) (merklePaths x t)
merklePaths x (Node _ l r) = map (Right (treeHash r):) (merklePaths x l) ++ map (Left (treeHash l):) (merklePaths x r)

showMerklePath [] = ""
showMerklePath (h:t) = case h of
    Left x -> ">" ++ showHash x ++ showMerklePath t
    Right x -> "<" ++ showHash x ++ showMerklePath t

instance (Show a) => Show (MerkleProof a) where
    show (MerkleProof a path) = "MerkleProof " ++ show a ++ " " ++ showMerklePath path
-- >>> buildProof 'i' $ buildTree "bitcoin"
-- Just (MerkleProof 'i' [Right 2575913374,Right 1777612924,Left 1644206767])

-- >>> showMerklePath $ getPath $ fromMaybe (MerkleProof 'i' []) $ buildProof 'i' $ buildTree "bitcoin"
-- "<0x9989519e<0x69f4387c>0x62009aaf"

-- >>> buildProof 'i' $ buildTree "bitcoin"
-- Just MerkleProof 'i' <0x9989519e<0x69f4387c>0x62009aaf

foldProof :: Hashable a => MerkleProof a -> Hash
foldProof (MerkleProof x path) = 
    foldr (\th h -> case th of
        Right hr -> hash (h, hr)
        Left hl -> hash (hl, h)) (hash x) path

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h p = foldProof p == h