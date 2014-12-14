-- testing file handles and typeclasses
import Data.Char
import System.IO

class HandleWrapper f where
  showFile :: f -> IO String
  matchesFile :: f -> IO Bool

data TextFileWrapper = WrappedTHandle (IO Handle)
data BinaryFileWrapper = WrappedBHandle (IO Handle)

instance HandleWrapper TextFileWrapper where
  showFile (WrappedTHandle h) = h >>= hGetContents
  matchesFile (WrappedTHandle h) = h >>= hGetChar >>= (return . isPrint)

instance HandleWrapper BinaryFileWrapper where
  showFile (WrappedBHandle _) = (return "Not printable")
  matchesFile (WrappedBHandle h) = h >>= hGetChar >>= (return . not . isPrint)

wrapHandleFromList :: HandleWrapper w => [w] -> IO Handle -> [w]
wrapHandleFromList wrapperList h = filter matchesFile (map (\w -> w h) wrapperList)

testTFH = WrappedBHandle $ openFile "b" ReadMode

main :: IO ()
main = do
  print =<< (matchesFile testTFH)
  putStrLn =<< (showFile testTFH)