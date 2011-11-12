module ParseDVI where

import DVIInstructions
import BinaryParse
import Data.Char
import qualified Data.Binary.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Lazy as BS
import Control.Applicative
import Control.Monad (liftM)


parseDVIOpcode :: Parse Int
parseDVIOpcode = fmap fromIntegral parseByte >>= \opcode ->
                   if opcode < 250
                   then return opcode
                   else fail $ "invalid opcode " ++ show opcode

parseDVIInstrBody :: Int -> Parse DVIInstr
parseDVIInstrBody opcode
    | opcode <  0    = fail "Negative opcode"
    | opcode <  128  = return (SetChar opcode)
    | opcode <  132  = (SetChar . (127 +)) <$> getUInteger (opcode - 127)
    | opcode == 132  = do height <- getInteger 4
                          width  <- getInteger 4
                          return (SetRule height width)
    | opcode <  137  = Put <$> getUInteger (opcode - 132)
    | opcode == 137  = do height <- getInteger 4
                          width  <- getInteger 4
                          return (PutRule height width)
    | opcode == 138  = return Nop
    | opcode == 139  = parseBop
    | opcode == 140  = return Eop
    | opcode == 141  = return Push
    | opcode == 142  = return Pop
    | opcode <  147  = RightI <$> getInteger (opcode - 142)
    | opcode == 147  = return W0
    | opcode <  152  = Wi <$> getInteger (opcode - 147)
    | opcode == 152  = return X0
    | opcode <  157  = Xi <$> getInteger (opcode - 152)
    | opcode <  161  = DownI <$> getInteger (opcode - 156)
    | opcode == 161  = return Y0
    | opcode <  166  = Yi <$> getInteger (opcode - 161)
    | opcode == 166  = return Z0
    | opcode <  171  = Zi <$> getInteger (opcode - 166)
    | opcode <  235  = return (FntNum (opcode - 171))
    | opcode <  239  = FntNum <$> getUInteger (opcode - 234)
    | opcode <  243  = parseSpecial (opcode - 238)
    | opcode <  247  = parseFontDef (opcode - 242)
    | opcode == 247  = parsePreamble
    | opcode == 248  = parsePostamble
    | opcode == 249  = parsePostPost
    | otherwise      = return (Put opcode)

parseBop :: Parse DVIInstr
parseBop = do
  c0 <- getInteger 4
  c1 <- getInteger 4
  c2 <- getInteger 4
  c3 <- getInteger 4
  c4 <- getInteger 4
  c5 <- getInteger 4
  c6 <- getInteger 4
  c7 <- getInteger 4
  c8 <- getInteger 4
  c9 <- getInteger 4
  p  <- getInteger 4
  return (Bop c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 p)

parseSpecial :: Int -> Parse DVIInstr
parseSpecial numbytes = do
  descrLength <- getUInteger numbytes
  descr       <- getString descrLength
  return (Special descrLength descr)

parseFontDef :: Int -> Parse DVIInstr
parseFontDef i = do fontnum          <- getUInteger i
                    checksum         <- getInteger 4
                    s                <- getInteger 4
                    d                <- getInteger 4
                    areaLength       <- getUInteger 1
                    fontNameLength   <- getUInteger 1
                    fullFontPath     <- getString (areaLength + fontNameLength)
                    return (FontDef fontnum checksum s d areaLength
                              fontNameLength fullFontPath)

parsePreamble :: Parse DVIInstr
parsePreamble = 
 do identifier <- getInteger 1
    assert (identifier == 2) "bad identifier byte"
    num         <- getInteger 4
    den         <- getInteger 4
    mag         <- getInteger 4
    descrLength <- getUInteger 1
    descr       <- getString descrLength
    return (Preamble identifier num den mag descrLength descr)

parsePostamble :: Parse DVIInstr
parsePostamble =
  do p    <- getInteger 4
     num  <- getInteger 4
     den  <- getInteger 4
     mag  <- getInteger 4
     l    <- getInteger 4
     u    <-  getInteger 4
     s    <- getUInteger 2
     t    <- getUInteger 2
     return (Postamble p num den mag l u s t)

parsePostPost :: Parse DVIInstr
parsePostPost =
  do q <- getInteger 4
     id <- getUInteger 1
     parseWhile (\bit -> fromIntegral bit == 223)
     return (PostPost q id)

parseDVIInstruction :: Parse DVIInstr
parseDVIInstruction = parseDVIOpcode >>= parseDVIInstrBody

parseDVIDocument :: Parse [DVIInstr] 
parseDVIDocument = do
  empty <- checkEmpty
  if empty
     then return []
     else do v <- parseDVIInstruction
             rest <- parseDVIDocument
             return (v : rest)

processdocument = do
  dvicontents <- BS.readFile "./labentry.dvi"
  print  (parse parseDVIDocument dvicontents)

--code for parsing n-byte integers in 2's complement representation 
bsToInteger :: (Num b) => BS.ByteString -> b
bsToInteger bytes =
     let l = fromIntegral (BS.length bytes) 
         get2cInteger = do
            bits <- G.getBytes l
            let r = BG.runBitGet bits (do
                      firstbit  <- liftM fromIntegral (BG.getAsWord8 1)
                      remainder <- liftM fromIntegral (BG.getAsWord32 (8*l -1))
                      return (-(2^(8*l-1))*firstbit + remainder) )
            case r of
              Left error -> fail error
              Right x    -> return x
     in  G.runGet get2cInteger bytes


getInteger :: (Num b) => Int -> Parse b
getInteger n = fmap bsToInteger (parseBytes n)

--code for parsing unsigned integers; actually probably uses 2c for n = 2,3
bsToUInteger :: (Num b) => BS.ByteString -> b
bsToUInteger bytes =
  case BS.length bytes of
    4 -> bsToInteger bytes  --because is allowed to be negative, see dvi spec
    1 -> (fromIntegral . (G.runGet G.getWord8))  bytes
    2 -> G.runGet (do firstChunk  <- fmap fromIntegral G.getWord8
                      secondChunk <- fmap fromIntegral G.getWord8
                      return (2^8*firstChunk + secondChunk)) bytes
    3 -> G.runGet (do firstChunk  <- fmap fromIntegral G.getWord8
                      secondChunk <- fmap fromIntegral G.getWord8
                      thirdChunk  <- fmap fromIntegral G.getWord8
                      return (2^16*firstChunk + 2^8*secondChunk + thirdChunk))
            bytes
    otherwise -> undefined

getUInteger :: (Num b) => Int -> Parse b
getUInteger n = fmap bsToUInteger (parseBytes n)

bsToString :: BS.ByteString -> [Char]
bsToString = map (chr . fromIntegral) . BS.unpack

getString :: Int -> Parse [Char]
getString n = fmap bsToString (parseBytes n)


