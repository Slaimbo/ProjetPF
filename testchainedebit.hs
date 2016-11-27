import Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits

fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ unpack contents
