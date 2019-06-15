module Day4 where

import           Crypto.Hash           (Digest, MD5, digestToHexByteString,
                                        hash)
import           Data.ByteString.Char8 (ByteString, append, isPrefixOf, length,
                                        pack, take)
import           Data.List             (find)
import           Data.Maybe            (fromMaybe)
import           Prelude               hiding (length, take)

part1 :: String -> Int
part1 = solution (pack "00000") . pack

part2 :: String -> Int
part2 = solution (pack "000000") . pack

solution prefix seed = fromMaybe 0 $ find (mined seed prefix) [1..]

mined seed prefix i = (prefix `isPrefixOf`) $ digestToHexByteString $ md5 $ ( seed `append` pack (show i))

md5 :: ByteString -> Digest MD5
md5 = hash
