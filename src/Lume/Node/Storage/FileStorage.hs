{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Storage.FileStorage (
  -- * Types
  GetFileStorageError (..),

  -- * Functions
  mkStorageFilePath,
  writeBlock,
  readBlock,
  getFileSize,
)
where

import Control.Monad (unless)
import Data.Binary (decode, decodeOrFail, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack)
import Data.Word (Word32)
import GHC.IO.Handle (Handle, hFileSize)
import GHC.IO.IOMode (IOMode (..))
import Lume.Core.Block (Block)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (SeekMode (AbsoluteSeek), hClose, hSeek, hTell, openBinaryFile)

data GetFileStorageError
  = FileStorageDecodeError Text
  | FileStorageInvalidPositionError
  deriving (Show, Eq)

mkStorageFilePath :: FilePath -> Word32 -> FilePath
mkStorageFilePath fp fileIdx =
  let fileName = replicate (5 - length (show fileIdx)) '0' <> show fileIdx
   in fp </> "blk_" <> fileName <> ".dat"
{-# INLINE mkStorageFilePath #-}

writeBlock :: FilePath -> Block -> IO Integer
writeBlock fp block = do
  exists <- doesFileExist fp
  unless exists $ writeFile fp ""
  withBinaryFile fp ReadWriteMode $ \h -> do
    hSeek h AbsoluteSeek =<< hFileSize h
    pos <- hTell h
    let serialized = encode block
        size = fromIntegral $ BSL.length serialized :: Word32
    BSL.hPut h $ encode size
    BSL.hPut h serialized
    pure pos

readBlock :: FilePath -> Integer -> IO (Either GetFileStorageError Block)
readBlock fp dataPos = withBinaryFile fp ReadMode $ \h -> do
  fileSize <- hFileSize h
  case checkPosition dataPos fileSize of
    Left err -> pure (Left err)
    Right () -> do
      hSeek h AbsoluteSeek dataPos
      sizeBS <- BSL.hGet h 4
      let size = decode sizeBS :: Word32
      blockBS <- BSL.hGet h (fromIntegral size)
      pure $ case decodeOrFail blockBS of
        Left (_, _, err) -> Left $ FileStorageDecodeError (pack err)
        Right (_, _, block) -> Right block
 where
  checkPosition :: Integer -> Integer -> Either GetFileStorageError ()
  checkPosition pos fileSize
    | pos >= fileSize = Left FileStorageInvalidPositionError
    | otherwise = Right ()

getFileSize :: FilePath -> IO Integer
getFileSize fp = do
  exists <- doesFileExist fp
  if exists
    then withBinaryFile fp ReadMode hFileSize
    else pure 0

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFile path mode action = do
  handle <- openBinaryFile path mode
  result <- action handle
  hClose handle
  pure result
