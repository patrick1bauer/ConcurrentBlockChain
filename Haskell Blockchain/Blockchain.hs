import Data.ByteString.UTF8 (fromString, toString)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.BytString.Base16 as Base16

data Transaction = Transaction { from :: String
                               , to   :: String
                               , data :: String
                               } deriving (Generic, Show)

data Block = Bloc { idx :: Int
                  , txs :: [Transaction]
                  , phash :: String
                  , hash  :: String
                  , nonce :: Maybe Int
                  } deriving (Generic, Show)

genesis :: Block
genesis = Block bidx btxs bphash bhash Nothing
    where bidx = 0
         btxs = [Transaction "Stacey" "Sierra" "Goodnight, baby!"]
         bphash = "000000000000000000000000000000000"
         bhash = ""

bAddTxs :: Block -> Transaction -> Block
bAddTxs (Block i tx ph h n) t = Block (i (tx ++ [t]) ph h n)

bHash :: Block -> String
bHash (Block i tx ph h _) = toString (Base16.encode digest)
      where bTxsStr = foldr ((++) . show) "" tx
            ctx = SHA256.updates SHA256.init $ fmap (fromString [bTxsStr, ph])
            digest = SHA256.finalize
