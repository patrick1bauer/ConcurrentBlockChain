Module SingleThreadedBlockChain where

import System.IO (readFile)
import Data.Time.Clock.System (getSystemTime)
import Data.ByteString.UTF8 (fromString, toString)
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16

data Block = Block { contents :: String
             , hash :: String
             , previousHash :: String
             , timeStamp :: String
             , nonce :: Maybe Int
             }

genesisBlock :: Block
genesisBlock = Block contents hash previousHash timeStamp Nothing
                 where 
                     contents = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
                     hash = ""
                     previousHash = "000000000000000000000000000000000"
                     timeStamp = getTime

newBlockChain :: Block -> Block
newBlockChain = mineBlock (genesisBlock) 0

getTime :: String
getTime = show $ getSystemTime

hashBlock :: Block -> String
hashBlock (Block contents hash previousHash timeStamp _) = toString $ Base16.encode digest
    where ctx = SHA256.updates SHA256.init $ fmap fromString [contents, prevHash, timestamp]
          digest = SHA256.finalize ctx 

mineBlock :: Block -> Int -> Block
mineBlock b@(Block c _ p t _) n = case head pow of
                                    '0' -> Block c blockHash p t (Just n)
                                    _   -> mineBlock b (n + 1)
    where blockHash = hashBlock b
          ctx = SHA256.updates SHA256.init (fmap fromString [blockHash, show n, p])
          pow = toString . Base16.encode $ SHA256.finalize ctx -- proof of work

getHash :: Block -> String
getHash (Block _ _ p _ _) = p

addBlock :: String -> String -> Block
addBlock p newContent = mineBlock (Block newContent "" p (getTime) Nothing) 0

makeBlockChain :: String -> [String] -> Int -> [Block]
makeBlockChain p [] n = [];
makeBlockChain p story 0 = [firstBlock] ++ makeBlockChain pHash story 1
                             where
                                 pHash = getHash firstBlock
                                 firstBlock = newBlockChain
makeBlockChain p (s:story) n = [newBlock] ++ makeBlockChain pHash story (n+1) 
                                 where  
                                    newBlock = addBlock p s 
                                    pHash = getHash newBlock
                    

