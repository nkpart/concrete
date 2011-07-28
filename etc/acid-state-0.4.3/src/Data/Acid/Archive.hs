{-
Format:
 |content length| crc16   | content |
 |8 bytes       | 2 bytes | n bytes |
-}
module Data.Acid.Archive
    ( Entry
    , Entries(..)
    , putEntries
    , packEntries
    , readEntries
    , entriesToList
    , entriesToListNoFail
    ) where

import Data.Acid.CRC

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Serialize.Get as Serialize
import Data.Serialize.Get hiding (Result(..))
import Data.Serialize.Builder
import Data.Monoid

type Entry = Lazy.ByteString
data Entries = Done | Next Entry Entries | Fail String
    deriving (Show)

entriesToList :: Entries -> [Entry]
entriesToList Done              = []
entriesToList (Next entry next) = entry : entriesToList next
entriesToList (Fail msg)        = fail msg

entriesToListNoFail :: Entries -> [Entry]
entriesToListNoFail Done              = []
entriesToListNoFail (Next entry next) = entry : entriesToListNoFail next
entriesToListNoFail Fail{}            = []

putEntry :: Entry -> Builder
putEntry content
    = putWord64le contentLength `mappend`
      putWord16le contentHash `mappend`
      fromLazyByteString content
    where contentLength = fromIntegral $ Lazy.length content
          contentHash   = crc16 content

putEntries :: [Entry] -> Builder
putEntries = mconcat . map putEntry

packEntries :: [Entry] -> Lazy.ByteString
packEntries = toLazyByteString . putEntries

readEntries :: Lazy.ByteString -> Entries
readEntries bs
    = worker (Lazy.toChunks bs)
    where worker [] = Done
          worker (x:xs)
              = check (runGetPartial readEntry x) xs
          check result more
              = case result of
                  Serialize.Done entry rest
                      | Strict.null rest    -> Next entry (worker more)
                      | otherwise           -> Next entry (worker (rest:more))
                  Serialize.Fail msg        -> Fail msg
                  Serialize.Partial cont    -> case more of
                                                 []     -> check (cont Strict.empty) []
                                                 (x:xs) -> check (cont x) xs

readEntry :: Get Entry
readEntry
    = do contentLength <- getWord64le
         contentChecksum <-getWord16le
         content <- getLazyByteString (fromIntegral contentLength)
         if crc16 content /= contentChecksum
           then fail "Invalid hash"
           else return content
