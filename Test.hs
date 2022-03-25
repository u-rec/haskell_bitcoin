module Main where
import HashTree
import Blockchain
import Hashable32
main = do
    putStr $ drawTree $ buildTree "bitcoin"
    print (buildProof 'i' $ buildTree "bitcoin")
    print (buildProof 'e' $ buildTree "bitcoin")
    print $ map showMerklePath  $ merklePaths 'i' $ buildTree "bitcoin"
    print $ map showMerklePath  $ merklePaths 'n' $ buildTree "bitcoin"
    let t = buildTree "bitcoin"
    let proof = buildProof 'i' t
    print $ verifyProof (treeHash t) <$> proof
    print $ verifyProof 0xbada55bb <$> proof
    print $ verifyChain [block1, block2]
    print $ VH <$> verifyChain [block2,block1,block0]
    print $ allMerklePaths $ buildTree "bitcoin"
    let charlie = hash "Charlie"
    let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
    print block
    print receipt
    print $ validateReceipt receipt (blockHdr block)