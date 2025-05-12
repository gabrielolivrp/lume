{-# LANGUAGE DerivingStrategies #-}

module Lume.Consensus.PoW (
  mineBlock,
  checkNonce,
) where

import Control.Lens
import Lume.Block.Types (Block, BlockHeader, bHeader, bNonce)
import Lume.Consensus.Difficulty (Target)
import Lume.Crypto.Hash (ToHash (toHash), hash2Integer)

checkNonce :: BlockHeader -> Target -> Bool
checkNonce header target =
  let hash = toHash header
   in hash2Integer hash < toInteger target
{-# INLINE checkNonce #-}

mineBlock :: Block -> Target -> Block
mineBlock block = go (block ^. bHeader)
 where
  go :: BlockHeader -> Target -> Block
  go header target =
    if checkNonce header target
      then block & bHeader .~ header
      else go (incrementNonce header) target

incrementNonce :: BlockHeader -> BlockHeader
incrementNonce header =
  let nonce = header ^. bNonce
      newNonce = nonce + 1
   in header & bNonce .~ newNonce
