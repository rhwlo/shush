-- test reading (and printing) binary data

import Control.Applicative
import Data.ByteString.Lazy as BS (readFile, unpack)
import Text.Printf (printf)

main :: IO ()
main = (unwords . listToHex . unpack) <$> (BS.readFile "b") >>= putStrLn
  where listToHex l = fmap (\n -> printf "%02x" n) l