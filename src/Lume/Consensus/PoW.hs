{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Lume.Consensus.PoW (
  -- * Types
  MineError (..),

  -- * Functions
  checkNonce,
  mineBlock,
) where

import Control.Lens
import Control.Monad.Except (MonadError (throwError))
import Lume.Block (Block, BlockHeader, bHeader, bNonce)
import Lume.Consensus.Difficulty (Target)
import Lume.Crypto.Hash (ToHash (toHash), hash2Integer)

data MineError
  = NonceOverflow
  deriving (Show)

checkNonce :: BlockHeader -> Target -> Bool
checkNonce header target =
  let hash = toHash header
   in hash2Integer hash < toInteger target
{-# INLINE checkNonce #-}

mineBlock :: (MonadError MineError m) => Block -> Target -> m Block
mineBlock block = go (block ^. bHeader)
 where
  go header target
    | checkNonce header target = pure $ block & bHeader .~ header
    | otherwise = do
        blockHeader <- incrementNonce header
        go blockHeader target

incrementNonce :: (MonadError MineError m) => BlockHeader -> m BlockHeader
incrementNonce header
  | header ^. bNonce >= maxBound = throwError NonceOverflow
  | otherwise =
      let !nonce = header ^. bNonce
          !newNonce = nonce + 1
       in pure $ header & bNonce .~ newNonce
