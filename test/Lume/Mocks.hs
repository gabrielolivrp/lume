{-# LANGUAGE OverloadedStrings #-}

module Lume.Mocks where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as M
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Lume.Block.Builder (hashBlock)
import Lume.Block.Types (Block (..), BlockHeader (..))
import Lume.Consensus.Difficulty (Bits, initialBits)
import Lume.Crypto.Address (Address (..))
import Lume.Crypto.Hash (Hash, ToHash (toHash), hash')
import Lume.Crypto.Signature (PublicKey (..), Signature (..), emptyPublicKey, emptySignature)
import Lume.Time.Timestamp (Timestamp (Timestamp))
import Lume.Transaction.Amount (Amount)
import Lume.Transaction.Types

----------------
-- Mock Hashes
----------------
mockHash1 :: Hash
mockHash1 = hash' "mock_hash"

mockHash2 :: Hash
mockHash2 = hash' "mock_hash_2"

mockHash3 :: Hash
mockHash3 = hash' "mock_hash_3"

mockHashFromInt :: Int -> Hash
mockHashFromInt n = hash' $ Char8.pack $ "mock_hash_" <> show n

-------------------
-- Mock Timestamp
-------------------
mockTimestamp1 :: Timestamp
mockTimestamp1 = Timestamp 1234567890

mockTimestamp2 :: Timestamp
mockTimestamp2 = Timestamp 1234567891

mockTimestampWithOffset :: Int -> Timestamp
mockTimestampWithOffset offset = Timestamp (1234567890 + fromIntegral offset)

-------------------
-- Mock Addresses
-------------------
mockAddress :: Int -> Address
mockAddress n = Address $ "lume_addr_" <> Text.pack ("lume_addr_" ++ show n)

mockAddress1 :: Address
mockAddress1 = mockAddress 1

mockAddress2 :: Address
mockAddress2 = mockAddress 2

mockAddress3 :: Address
mockAddress3 = mockAddress 2

-----------------------------
-- Mock Keys and Signatures
-----------------------------
mockPubKey1 :: PublicKey
mockPubKey1 = PublicKey "mock_pubkey_10000000000000000000"

mockPubKey2 :: PublicKey
mockPubKey2 = PublicKey "mock_pubkey_20000000000000000000"

mockPubKey3 :: PublicKey
mockPubKey3 = PublicKey "mock_pubkey_30000000000000000000"

mockSignature1 :: Signature
mockSignature1 = Signature "mock_signature_10000000000000000"

mockSignature2 :: Signature
mockSignature2 = Signature "mock_signature_20000000000000000"

mockSignature3 :: Signature
mockSignature3 = Signature "mock_signature_30000000000000000"

---------------------
-- Mock Transaction
---------------------
mockTxOut1 :: TxOut
mockTxOut1 = TxOut mockAddress1 1000

mockTxOut2 :: TxOut
mockTxOut2 = TxOut mockAddress2 2000

mockTxOut3 :: TxOut
mockTxOut3 = TxOut mockAddress3 3000

mockTxIn :: Hash -> Word64 -> Signature -> PublicKey -> TxIn
mockTxIn txHash idx sig pubKey =
  TxIn
    { _txInPrevOut = Outpoint txHash idx
    , _txInSignature = sig
    , _txInPubKey = pubKey
    }

txIn1 :: TxIn
txIn1 = mockTxIn mockHash1 0 emptySignature emptyPublicKey

txIn2 :: TxIn
txIn2 = mockTxIn mockHash2 0 emptySignature emptyPublicKey

txIn3 :: TxIn
txIn3 = mockTxIn mockHash3 1 emptySignature emptyPublicKey

mockTx1 :: Tx
mockTx1 = Tx [] [mockTxOut1] 1

mockTx2 :: Tx
mockTx2 = Tx [txIn1] [mockTxOut2] 1

mockTx3 :: Tx
mockTx3 = Tx [txIn1, txIn2] [mockTxOut1, mockTxOut2] 1

mockCoinbaseTx :: Word32 -> [TxOut] -> Tx
mockCoinbaseTx version outputs =
  Tx
    { _txVersion = version
    , _txIn = []
    , _txOut = outputs
    }

mockCoinbase :: Tx
mockCoinbase = mockCoinbaseTx 1 [TxOut mockAddress1 5000]

mockOutpoint :: Tx -> Word64 -> Outpoint
mockOutpoint tx = Outpoint (toHash tx)

mockOutpointTx1 :: Outpoint
mockOutpointTx1 = mockOutpoint mockTx1 0

---------------
-- Mock UTXOs
---------------
mockUTXO :: Hash -> Word64 -> Address -> Amount -> UTXO
mockUTXO txId idx addr val =
  UTXO
    { _utxoId = txId
    , _utxoIdx = idx
    , _utxoOwner = addr
    , _utxoValue = val
    }

utxo1 :: UTXO
utxo1 = mockUTXO mockHash1 0 mockAddress1 1000

utxo2 :: UTXO
utxo2 = mockUTXO mockHash2 0 mockAddress2 2000

utxo3 :: UTXO
utxo3 = mockUTXO mockHash3 1 mockAddress3 3000

mockUtxoSet :: [UTXO] -> UtxoSet
mockUtxoSet utxos =
  UtxoSet $
    M.fromList [(Outpoint (_utxoId u) (_utxoIdx u), u) | u <- utxos]

mockBasicUtxoSet :: UtxoSet
mockBasicUtxoSet = mockUtxoSet [utxo1, utxo2, utxo3]

----------------
-- Mock Blocks
----------------
mockBlockHeader :: Word32 -> Word32 -> Hash -> Hash -> Timestamp -> Bits -> BlockHeader
mockBlockHeader version nonce merkleRoot prevHash timestamp bits =
  BlockHeader
    { _bVersion = version
    , _bNonce = nonce
    , _bMerkleRoot = merkleRoot
    , _bHashPrevBlock = prevHash
    , _bTimestamp = timestamp
    , _bBits = bits
    }

mockBlock :: BlockHeader -> Word64 -> [Tx] -> Block
mockBlock header height txs =
  Block
    { _bHeader = header
    , _bHeight = height
    , _bTxs = Txs txs
    }

mockGenesisBlockHeader :: BlockHeader
mockGenesisBlockHeader = mockBlockHeader 1 0 mockHash1 mockHash2 mockTimestamp1 initialBits

mockGenesisBlock :: Block
mockGenesisBlock = mockBlock mockGenesisBlockHeader 0 []

mockBlockHeader1 :: BlockHeader
mockBlockHeader1 =
  BlockHeader
    { _bVersion = 1
    , _bNonce = 0
    , _bMerkleRoot = mockHash1
    , _bTimestamp = mockTimestamp1
    , _bBits = initialBits
    , _bHashPrevBlock = hashBlock mockGenesisBlock
    }

mockBlock1 :: Block
mockBlock1 =
  Block
    { _bHeader = mockBlockHeader1
    , _bHeight = 1
    , _bTxs = Txs []
    }

mockBlockHeader2 :: BlockHeader
mockBlockHeader2 =
  BlockHeader
    { _bVersion = 1
    , _bNonce = 0
    , _bMerkleRoot = mockHash2
    , _bTimestamp = mockTimestamp2
    , _bBits = initialBits
    , _bHashPrevBlock = hashBlock mockBlock1
    }

mockBlock2 :: Block
mockBlock2 =
  Block
    { _bHeader = mockBlockHeader2
    , _bHeight = 2
    , _bTxs = Txs []
    }
