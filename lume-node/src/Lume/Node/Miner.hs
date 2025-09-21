module Lume.Node.Miner (
  module Lume.Node.Miner.PoW,
  startMiner,
)
where

import Control.Distributed.Process (Process, say)
import Lume.Node.Config (Config)
import Lume.Node.Miner.PoW
import Lume.Node.Network.State (NodeContext)

startMiner :: Config -> NodeContext -> Process ()
startMiner _config _context = say "Starting miner..."
