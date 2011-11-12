module BinaryParse where

import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Word
import Control.Monad.State
import Control.Applicative

data ParseState = ParseState {
      string :: BS.ByteString,
      offset :: Int64
      } deriving (Show)

type Parse = State ParseState


parse :: Parse a -> BS.ByteString -> (a, ParseState)
parse parser bytes =
   runState parser (ParseState bytes 0)

parseByte :: Parse Word8
parseByte = 
   get >>= \initState ->
   case BS.uncons (string initState) of
     Nothing ->
       fail "no more input"
     Just (byte, remainder) ->
       put newState  >>
       return byte
      where newState  = initState {string = remainder, offset = newOffset}
            newOffset = offset initState + 1

checkEmpty :: Parse Bool
checkEmpty = (BS.null . string) <$> get

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . BS.uncons . string) <$> get

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) >>= \mp ->
               if mp == Just True
               then parseByte >>= \b ->
                 (b:) <$> parseWhile p
               else return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

assert :: Bool -> String -> Parse ()
assert True _       = return ()
assert False errmsg = fail errmsg

parseBytes :: Int -> Parse BS.ByteString
parseBytes n =
    get >>= \st ->
    let n' = fromIntegral n
        (h, t) = BS.splitAt n' (string st)
        st' = st {offset = offset st + BS.length h, string = t}
    in  put st' >>
        assert (BS.length h == n') "end of input" >>
        return h

--Seems to work exactly the same as the above version, but until they get
--back to me, I will stick with the RWH version.
parseBytes2 :: Int -> Parse BS.ByteString
parseBytes2 n =
   if n == 1
   then fmap BS.singleton parseByte
   else parseByte >>= \byte ->
     BS.append (BS.singleton byte) <$> parseBytes (n-1)

