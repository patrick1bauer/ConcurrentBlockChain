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
                     timeStamp = show $ getSystemTime

newBlockChain :: Block -> [Block]
newBlockChain = [genesisBlock]

