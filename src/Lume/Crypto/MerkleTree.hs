{-# LANGUAGE ImportQualifiedPost #-}

module Lume.Crypto.MerkleTree (
  -- * Types
  MerkleTree (..),
  MerkleProof (..),
  ProofElem (..),
  Side (..),

  -- * Functions
  fromList,
  generateProof,
  verifyProof,
  fromListWithRoot,
  getMerkleHash,
)
where

import Data.ByteString qualified as BS
import Lume.Crypto.Hash (Hash, ToHash (toHash), hash', toRawBytes)

data MerkleTree a
  = MerkleLeaf a Hash
  | MerkleNode Hash (MerkleTree a) (MerkleTree a)

newtype MerkleProof = MerkleProof [ProofElem]

data ProofElem = ProofElem
  { nodeRoot :: Hash
  , siblingRoot :: Hash
  , nodeSide :: Side
  }

data Side = L | R
  deriving (Eq)

fromList :: (ToHash a) => [a] -> MerkleTree a
fromList xs = build (map mkMerkleLeaf xs)
 where
  build [t] = t
  build ts = build (pairUp ts)

  pairUp (x : y : rest) = mkMerkleNode x y : pairUp rest
  pairUp [x] = [mkMerkleNode x x]
  pairUp [] = []

generateProof :: (ToHash a) => MerkleTree a -> Hash -> MerkleProof
generateProof mtree targetHash =
  case findPath mtree of
    Nothing -> MerkleProof []
    Just path -> MerkleProof (reverse path)
 where
  findPath :: (ToHash a) => MerkleTree a -> Maybe [ProofElem]
  findPath (MerkleLeaf _ h)
    | h == targetHash = Just []
    | otherwise = Nothing
  findPath (MerkleNode _ left right) =
    case findPath left of
      Just path ->
        pure (ProofElem (getMerkleHash left) (getMerkleHash right) L : path)
      Nothing -> do
        path <- findPath right
        pure (ProofElem (getMerkleHash right) (getMerkleHash left) R : path)

verifyProof :: MerkleProof -> Hash -> Hash -> Bool
verifyProof (MerkleProof proofElems) treeRoot = go proofElems
 where
  go [] proofRoot = proofRoot == treeRoot
  go (p : ps) proofRoot
    | proofRoot /= nodeRoot p = False
    | otherwise = go ps (hashElem p)

  hashElem (ProofElem pRoot sibRoot side)
    | side == L = hashPair pRoot sibRoot
    | otherwise = hashPair sibRoot pRoot

fromListWithRoot :: (ToHash a) => [a] -> (MerkleTree a, Hash)
fromListWithRoot xs = let tree = fromList xs in (tree, getMerkleHash tree)
{-# INLINE fromListWithRoot #-}

getMerkleHash :: (ToHash a) => MerkleTree a -> Hash
getMerkleHash (MerkleLeaf _ hash) = hash
getMerkleHash (MerkleNode hash _ _) = hash
{-# INLINE getMerkleHash #-}

hashPair :: Hash -> Hash -> Hash
hashPair h1 h2 = hash' (BS.concat [toRawBytes h1, toRawBytes h2])
{-# INLINE hashPair #-}

mkMerkleLeaf :: (ToHash a) => a -> MerkleTree a
mkMerkleLeaf x = let hash = toHash x in MerkleLeaf x hash
{-# INLINE mkMerkleLeaf #-}

mkMerkleNode :: (ToHash a) => MerkleTree a -> MerkleTree a -> MerkleTree a
mkMerkleNode left right =
  let hash = hashPair (getMerkleHash left) (getMerkleHash right)
   in MerkleNode hash left right
{-# INLINE mkMerkleNode #-}
