module Db.Db where

import Data.Binary.Builder as Bld
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int32, Int64)

type UserId = Int64
type Username = String
type Email = String

type SizeInBytes = Int64

idSize ::SizeInBytes
idSize = 8

idOffset = 0

usernameSize ::SizeInBytes
usernameSize = 32

usernameOffset = idOffset + idSize;

emailSize ::SizeInBytes
emailSize = 255

emailOffset = usernameOffset + usernameSize;

rowSize = idSize + usernameSize + emailSize;

resize :: B.ByteString -> SizeInBytes -> B.ByteString
resize text size = B.append prefix text
  where
    prefix = B.pack $ Prelude.replicate (fromIntegral size - B.length text) 0

data Row = Row
  { rid :: UserId
  , name :: Username
  , email :: Email
  } deriving (Show, Eq)

class Serializable a where
  serialize :: a -> LB.ByteString
  deserialize :: LB.ByteString -> a

instance Serializable Row where
  serialize row = let i = Bld.putInt64be $ rid row
                      u = Bld.fromByteString $ resize (C.pack $ name row) usernameSize
                      e = Bld.fromByteString $ resize (C.pack $ email row) emailSize
                      total = Bld.append (Bld.append i u) e
                      result = Bld.toLazyByteString total
                  in result

  deserialize = runGet $ do
      index <- Get.getInt64be
      rem <- Get.getRemainingLazyByteString
      let name = C.unpack $
                 LB.toStrict $
                 LB.dropWhile (== 0) $
                 LB.take usernameSize rem
          email = C.unpack $
                  LB.toStrict $
                  LB.dropWhile (== 0) $
                  LB.take emailSize $
                  LB.drop usernameSize rem
      pure $ Row index name email

pageSize :: Int64
pageSize = 4096;

tableMaxPages :: Int64
tableMaxPages = 100;

rowsPerPage :: Int64
rowsPerPage = div pageSize rowSize;

tableMaxRows :: Int64
tableMaxRows = rowsPerPage * tableMaxPages;

data Table = Table
  { pages :: [LB.ByteString]
  , numberOfRows :: Int64
  } deriving (Show)

newTable :: Table
newTable = Table (Prelude.replicate (fromIntegral tableMaxPages) LB.empty) 0

readSlot :: Table -> Int64 -> Row
readSlot table rowNumber =
  let pageNumber = div rowNumber rowsPerPage
      page = pages table !! fromIntegral pageNumber
      rowOffset = mod rowNumber rowsPerPage
      row = LB.take rowSize $ LB.drop (rowOffset * rowSize) page
  in deserialize row

writeSlot :: Table -> Int64 -> Row -> Table
writeSlot table rowNumber row =
  let pageNumber = div rowNumber rowsPerPage
      (tablePrefix, page:tableSuffix) = splitAt (fromIntegral pageNumber) (pages table)
      rowOffset = mod rowNumber rowsPerPage
      prefix = LB.take (rowOffset  * rowSize) page
      suffix = LB.drop ((rowOffset + 1) * rowSize) page
      newPage = LB.concat [prefix, serialize row, suffix]
      newPages = concat [tablePrefix, [newPage], tableSuffix]
      newNumberOfRows = numberOfRows table + 1
  in Table newPages newNumberOfRows
