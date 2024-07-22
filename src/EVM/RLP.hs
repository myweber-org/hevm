module EVM.RLP where

import EVM.Types
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Witch (into, unsafeInto)

data RLP = BS ByteString | List [RLP] deriving Eq

instance Show RLP where
  show (BS str) = show (ByteStringS str)
  show (List list) = show list

slice :: Int -> Int -> ByteString -> ByteString
slice offset size bs = BS.take size $ BS.drop offset bs

-- helper function returning (the length of the prefix, the length of the content, isList boolean, optimal boolean)
itemInfo :: ByteString -> (Int, Int, Bool, Bool)
itemInfo bs | bs == mempty = (0, 0, False, False)
            | otherwise = case BS.head bs of
  x | 0 <= x && x < 128   -> (0, 1, False, True) -- directly encoded byte
  x | 128 <= x && x < 184 -> (1, into x - 128, False, (BS.length bs /= 2) || (127 < (BS.head $ BS.drop 1 bs))) -- short string
  x | 184 <= x && x < 192 -> (1 + pre, len, False, (len > 55) && BS.head (BS.drop 1 bs) /= 0) -- long string
    where pre = into $ x - 183
          -- TODO: unsafeInto fails: cabal run test -- -p 'rlp' --quickcheck-replay=413899
          len = fromIntegral $ word $ slice 1 pre bs
  x | 192 <= x && x < 248 -> (1, into $ x - 192, True, True) -- short list
  x                       -> (1 + pre, len, True, (len > 55) && BS.head (BS.drop 1 bs) /= 0) -- long list
    where pre = into $ x - 247
          -- TODO: unsafeInto fails: cabal run test -- -p 'rlp' --quickcheck-replay=146332
          len = fromIntegral $ word $ slice 1 pre bs

rlpdecode :: ByteString -> Maybe RLP
rlpdecode bs =
  if optimal && pre + len == BS.length bs then
    if isList then do
      items <- mapM (\(s, e) -> rlpdecode $ slice s e content) (rlplengths content 0 len)
      Just (List items)
    else
      Just (BS content)
  else Nothing
  where (pre, len, isList, optimal) = itemInfo bs
        content = BS.drop pre bs

rlplengths :: ByteString -> Int -> Int -> [(Int,Int)]
rlplengths bs acc top =
  if acc < top then
    let (pre, len, _, _) = itemInfo bs
    in (acc, pre + len) : rlplengths (BS.drop (pre + len) bs) (acc + pre + len) top
  else []

rlpencode :: RLP -> ByteString
rlpencode (BS bs) = if BS.length bs == 1 && BS.head bs < 128 then bs
                    else encodeLen 128 bs
rlpencode (List items) = encodeLen 192 (mconcat $ map rlpencode items)

encodeLen :: Int -> ByteString -> ByteString
encodeLen offset bs =
  if BS.length bs <= 55 then prefix (BS.length bs) <> bs
  else prefix lenLen <> lenBytes <> bs
  where
    lenBytes = asBE $ BS.length bs
    prefix n = BS.singleton $ unsafeInto $ offset + n
    lenLen = BS.length lenBytes + 55

rlpList :: [RLP] -> ByteString
rlpList n = rlpencode $ List n

octets :: W256 -> ByteString
octets x =
  BS.pack $ dropWhile (== 0) [fromIntegral (shiftR x (8 * i)) | i <- reverse [0..31]]

octetsFull :: Int -> W256 -> ByteString
octetsFull n x =
  BS.pack $ [fromIntegral (shiftR x (8 * i)) | i <- reverse [0..n]]

octets160 :: Addr -> ByteString
octets160 x =
  BS.pack $ dropWhile (== 0) [fromIntegral (shiftR x (8 * i)) | i <- reverse [0..19]]

rlpWord256 :: W256 -> RLP
rlpWord256 0 = BS mempty
rlpWord256 n = BS $ octets n

rlpWordFull :: W256 -> RLP
rlpWordFull = BS . octetsFull 31

rlpAddrFull :: Addr -> RLP
rlpAddrFull = BS . octetsFull 19 . into

rlpWord160 :: Addr -> RLP
rlpWord160 0 = BS mempty
rlpWord160 n = BS $ octets160 n
