module ParseDVI where

import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import Data.Char
import Control.Applicative
import DVIInstructions

data ParseState = ParseState {
      string :: BS.ByteString,
      offset :: Int64
      } deriving (Show)

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
      }

instance Monad Parse where
  return a = Parse (\s -> Right (a, s))
  firstParser >>= secondParser = Parse chainedParser
    where chainedParser initState =
            case runParse firstParser initState of
             Left errmsg -> Left errmsg
             Right (firstResult, newState) ->
              runParse (secondParser firstResult) newState 
  fail errmsg = Parse $ \s -> Left $
                "byte offset" ++ show (offset s) ++ ":" ++ errmsg

instance Functor Parse where
  fmap f parser = parser >>= \result -> return (f result)

parse :: Parse a -> BS.ByteString -> Either String a
parse parser bytes =
   case runParse parser (ParseState bytes 0) of
     Left err -> Left err
     Right (result, _) -> Right result

parseByte :: Parse Word8
parseByte = 
   getState >>= \initState ->
   case BS.uncons (string initState) of
     Nothing ->
       fail "no more input"
     Just (byte, remainder) ->
       putState newState    >>
       return byte
      where newState  = initState {string = remainder, offset = newOffset}
            newOffset = offset initState + 1

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . BS.uncons . string) <$> getState

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
    getState >>= \st ->
    let n' = fromIntegral n
        (h, t) = BS.splitAt n' (string st)
        st' = st {offset = offset st + toEnum(BS.length h)::Int64, string = t}
    in  putState st' >>
        assert (BS.length h == n') "end of input" >>
        return h

--Seems to work exactly the same as the above version, but until they get
--back to me, I will stick with the RWH version.
parseBytes2 :: Int -> Parse BS.ByteString
parseBytes2 n =
   if n == 1
   then fmap BS.singleton parseByte
   else parseByte >>= \byte ->
     (BS.append (BS.singleton byte)) <$> parseBytes (n-1)


parseDVIOpcode :: Parse Int
parseDVIOpcode = fmap fromIntegral parseByte >>= \opcode ->
                   if opcode < 250
                   then return opcode
                   else fail $ "invalid opcode " ++ show opcode

parseDVIInstruction :: Int -> Parse DVIInstr
parseDVIInstruction n
    | n <  128   = undefined
    | n == 247   = parsePreamble
    | otherwise  = undefined

parsePreamble :: Parse DVIInstr
parsePreamble = 
    fmap fromIntegral parseByte >>= \identifier -> 
    assert (identifier == 2) "bad identifier byte" >>
    return (Preamble identifier 0 0 0 0 "testpreamble!")

parseDVIDocument :: Parse DVIInstr
parseDVIDocument = parseDVIOpcode >>= \opcode ->
                         assert (opcode == 247) "bad first opcode" >>
                         parseDVIInstruction opcode
processdocument = do
  dvicontents <- BS.readFile "./labentry.dvi"
  print (parse parseDVIDocument dvicontents)
  
{-

parseDviInstr :: BS.ByteString -> Maybe (DviInstr, BS.ByteString)
parseDviInstr s = getDviOpCode s >>? \(opcode, s1) ->
                   case opcode of
                     247 -> Just ((Preamble 0 0 0 0 1 "testpreamble2!"), s1)
                     _   -> Nothing

getDviOpCode :: BS.ByteString -> Maybe (Int, BS.ByteString)
getDviOpCode s
| opcode < 256
    = Just (opcode, BS.tail s)
  | otherwise
    = Nothing
  where opcode = fromIntegral $ BS.head s

parseDviInstr :: BS.ByteString -> Maybe (DviInstr, BS.ByteString)
parseDviInstr s = case getDviOpCode s of
                    Nothing -> Nothing
                    Just (opcode, s1) ->
                      case opcode of
                        247 -> Just ((Preamble 0 0 0 0 1 "testpreamble!"), s1)
                        _   -> Nothing
-}
--parsedviOpcode = do
--  dvicontents <- B.readFile "./labentry.dvi"
--  print $ B.unpack $ B.take 4 dvicontents


--getDVIPreamble :: Get [Word8]
--getDVIPreamble = do
--  opcode <- getWord8
--  dvibyte <- getWord8
--  return [opcode, dvibyte]
