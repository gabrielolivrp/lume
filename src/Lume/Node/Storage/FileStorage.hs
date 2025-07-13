{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Node.Storage.FileStorage (
  -- * Types
  GetFileStorageError (..),
  FileIndex,
  BlockPosition,

  -- * Functions
  mkFileName,
  mkDirectoryPath,
  writeBlock,
  maxFileSize,
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO (SeekMode (AbsoluteSeek), hClose, hSeek, hTell, openBinaryFile)

type FileIndex = Word32

type BlockPosition = Integer

data GetFileStorageError
  = FileStorageDecodeError Text
  | FileStorageInvalidPositionError
  deriving (Show, Eq)

maxFileSize :: Integer
maxFileSize = 128 * 1024 * 1024 -- 128 MB
{-# INLINE maxFileSize #-}

mkFileName :: FileIndex -> String
mkFileName fileIdx =
  let fileName = replicate (5 - length (show fileIdx)) '0' <> show fileIdx
   in "blk_" <> fileName <> ".dat"

mkDirectoryPath :: FilePath -> FilePath
mkDirectoryPath basePath = basePath </> "data"
{-# INLINE mkDirectoryPath #-}

writeBlock :: FilePath -> FileIndex -> Block -> IO BlockPosition
writeBlock basePath index block = do
  let dir = mkDirectoryPath basePath
  directoryExists <- doesDirectoryExist dir
  createDirectoryIfMissing directoryExists dir

  let fp = dir </> mkFileName index
  fileExists <- doesFileExist fp
  unless fileExists $ writeFile fp ""

  withBinaryFile fp ReadWriteMode $ \h -> do
    hSeek h AbsoluteSeek =<< hFileSize h
    pos <- hTell h
    let serialized = encode block
        size = fromIntegral $ BSL.length serialized :: Word32
    BSL.hPut h $ encode size
    BSL.hPut h serialized
    pure pos

readBlock :: FilePath -> FileIndex -> BlockPosition -> IO (Either GetFileStorageError Block)
readBlock basePath index dataPos = do
  let fp = mkDirectoryPath basePath </> mkFileName index
  withBinaryFile fp ReadMode $ \h -> do
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

getFileSize :: FilePath -> FileIndex -> IO Integer
getFileSize basePath index = do
  let fp = mkDirectoryPath basePath </> mkFileName index
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
