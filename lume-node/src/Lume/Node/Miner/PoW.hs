{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Lume.Node.Miner.PoW (
  -- * Types
  MineError (..),

  -- * Functions
  checkNonce,
  mineBlock,
) where

import Control.Lens
import Lume.Core.Block
import Lume.Core.Crypto.Hash (ToHash (toHash), hash2Integer)

data MineError
  = NonceOverflow
  deriving (Show)

checkNonce :: BlockHeader -> Target -> Bool
checkNonce header target =
  let !hash = toHash header
   in hash2Integer hash < toInteger target
{-# INLINE checkNonce #-}

mineBlock :: Block -> Target -> Either MineError Block
mineBlock block = go (block ^. bHeader)
 where
  go header target
    | checkNonce header target = Right (block & bHeader .~ header)
    | otherwise = do
        blockHeader <- incrementNonce header
        go blockHeader target

incrementNonce :: BlockHeader -> Either MineError BlockHeader
incrementNonce header
  | header ^. bNonce >= maxBound = Left NonceOverflow
  | otherwise =
      let !nonce = header ^. bNonce
          !newNonce = nonce + 1
       in Right (header & bNonce .~ newNonce)
