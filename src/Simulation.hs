{-# LANGUAGE ScopedTypeVariables #-}

module Simulation where

import Constants
import Blockchain
import Account
import Transaction
import Block

import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W
import Data.Maybe
import Data.List
import System.Random
import qualified Data.Map as Map


data SimulationData = SimulationData {
    timestamp :: Timestamp,
    deadline :: Timestamp,
    maxConnectionsPerNode :: Int,
    addNodeAvgGap :: Int,
    simulationId :: Int,
    codeLibrary :: [VerifiableTransactionPayload]
}

incTimestamp :: SimulationData -> SimulationData
incTimestamp sd = sd {timestamp = t + 1} where t = timestamp sd

notExpired :: SimulationData -> Bool
notExpired sd = (timestamp sd) < (deadline sd)

simpleGen :: SimulationData -> StdGen
simpleGen sd = mkStdGen $ (simulationId sd) * (timestamp sd + 1)

nodeGen :: SimulationData -> Node ->  StdGen
nodeGen sd nd = mkStdGen $ (simulationId sd) * (timestamp sd + 1) * (nodeId nd)

createNode :: Account -> Node
createNode acc = Node {localView = genesisView, pendingTxs = [], pendingBlocks = [], openBlocks = [genesisBlock],  account = acc}

rescan :: Node -> [(Block,Block)] -> Node
rescan = pushBlocks
                              --{localView = genesisView, pendingTxs = [], pendingBlocks = [], openBlocks = [genesisBlock], bestBlock = genesisBlock}


-----------Bootstrapping functions----------------------


earlyInvestors :: [Account]
earlyInvestors = map (\i -> generateAccount $ mkStdGen i) []


genesisBlock :: Block
genesisBlock = block where
    block = Block {transactions = [],  blockTimestamp = 0, baseTarget = initialBaseTarget, totalDifficulty = 0.0,
                         generator = godAccount, generationSignature = B.replicate 64 0}


genesisView :: LocalView
genesisView = LocalView {blockBalances = Map.singleton genesisBlock genBalances, blockTheory = Map.empty,
                         blockTree = Map.empty,
                         blockTransactions = Map.singleton genesisBlock (transactions genesisBlock),
                         bestBlock = genesisBlock,
                         diffThreshold = 0}
    where
        genBalances = fst $ processBlock genesisBlock (initialBalances, Map.empty)
        initialBalances = Map.singleton godAccount systemBalance


genesisState :: Network
genesisState = Network {nodes = [createNode $ godAccount], connections = Map.empty}


-----------Helper functions----------------------

randomBytes :: Int -> StdGen -> [W.Word8]
randomBytes 0 _ = []
randomBytes count g = fromIntegral value:randomBytes (count - 1) nextG
                      where (value, nextG) = next g

randomByteString :: Int -> StdGen -> B.ByteString
randomByteString count g = B.pack $ randomBytes count g


generatePK :: StdGen -> B.ByteString
generatePK gen = randomByteString 32 gen


accountByPK :: B.ByteString -> Account
accountByPK pk = Account {publicKey = pk, tfdepth = tfDepth}

generateAccount :: StdGen -> Account
generateAccount gen = accountByPK $ generatePK gen


-----------Simulating functions ----------------------

--for now 1 node == 1 account
addAccount :: Network -> Network
addAccount _ = error "not impl"

generateConnections :: SimulationData -> Network -> Network
generateConnections sd network = network {connections = updCons} where
    ns = nodes network
    nsCount = length ns
    idscons = connections network
    updCons = foldl (\idc n -> let out = outgoingConnectionsIds network n in
            if (length out) < (min (maxConnectionsPerNode sd)  nsCount-1)
                then
                    let gen = nodeGen sd n in
                    let rndNode = ns !! (fst $ randomR (0, nsCount - 1) gen) in
                        if (rndNode /=  n) && (notElem (nodeId rndNode) out)
                            then Map.insert n ((nodeId rndNode):out) idc
                            else idc
                else idc
        ) idscons ns

-- todo: move 60 to constants
dropConnections :: SimulationData -> Network -> Network
dropConnections sd network = case (timestamp sd) `mod` 60 of
            0 -> do
                let cons = connections network in
                    let updCons = foldl (\cs n -> let out = outgoingConnectionsIds network n in
                            if (length out) > ((maxConnectionsPerNode sd) `div` 2)
                                then Map.insert n (tail out) cs
                                else cs ) cons (nodes network) in
                    network {connections = updCons}
            _ -> network


randomNeighbour :: SimulationData -> Node -> Network -> Maybe Node
randomNeighbour sd node network = if nbcnt == 0 then Nothing
                                    else let idx = fst $ randomR (0, nbcnt - 1) (nodeGen sd node) in
                                         Just $ neighbours !! idx
                                  where
                                    neighbours = outgoingConnections network node
                                    nbcnt = length neighbours


addNode :: SimulationData -> Network -> Network
addNode sd initNetwork = case rnd  of
                            1 -> initNetwork {nodes = (createNode $ generateAccount gen):(nodes initNetwork)}
                            _ -> initNetwork
                        where
                            gen = simpleGen sd
                            rnd::Int = fst $ randomR (0, 100) gen

makePairs :: BlockChain -> [(Block, Block)]
makePairs [] = []
makePairs [b] = []
makePairs (b1:(b2:bs)) = (b1,b2):(makePairs (b2:bs))

-- rewrite with trees !
downloadBlocksFrom :: Node -> Node -> Network -> Network
downloadBlocksFrom node otherNode network = updNetwork
        where
        chain = bestChain node
        otherChain = bestChain otherNode
        updNetwork = if cumulativeNodeDifficulty otherNode > cumulativeNodeDifficulty node then
                       let common = commonChain chain otherChain in
                       let otherChainLength = length otherChain in
                       let commonChainLength = length common in
                       let blocksNumToDl = min (otherChainLength - commonChainLength) maxBlocksFromPeer in
                       let newBlocks = drop (commonChainLength-1) $ take (commonChainLength + blocksNumToDl) otherChain in
                       -- not very efficient drops and takes if full rescan, easier to rescan (take x otherChain)
                       -- now it is simplified to partial rescan
                       let modifiedNode = rescan node (makePairs newBlocks) in
                       updateNode modifiedNode network
                     else network


downloadBlocks :: SimulationData -> Node -> Network -> Network
downloadBlocks sd node network = resNetwork
    where
        mbUpdNeighbour = randomNeighbour sd node network
        resNetwork = case mbUpdNeighbour of
                Just neighbour -> downloadBlocksFrom node neighbour network
                Nothing -> network


downloadBlocksNetwork :: SimulationData -> Network ->  Network
downloadBlocksNetwork sd network = foldl (\nw n -> downloadBlocks sd n nw) network (nodes network)

sendTransactionsOutNode ::  Node -> Node -> Network -> Network
sendTransactionsOutNode node neighbour network =
                                        let txsToSend = pendingTxs node in
                                        let otherTxs = pendingTxs neighbour in
                                        let newTxs' = filter (\tx -> notElem tx otherTxs) txsToSend in
                                        let neiProcTxs = procTxs neighbour in
                                        let newTxs = filter (\tx -> notElem tx neiProcTxs) newTxs' in
                                        let updTxs = otherTxs ++ newTxs in
                                         updateNode neighbour {pendingTxs = updTxs} network

sendTransactionsOut :: SimulationData -> Node -> Network -> Network
sendTransactionsOut sd node network = case randomNeighbour sd node network of
                     Just neighbour -> sendTransactionsOutNode node neighbour network 
                     Nothing -> network


--it's probably no need to filter
propagateTransactions :: SimulationData -> Network ->  Network
propagateTransactions sd network = foldl (\nws sndr -> sendTransactionsOut sd sndr nws) network (nodes network)
           -- where senders = filter (\n -> (length $ pendingTxs n) > 0) (nodes network)


-- todo: regulate whether to send and whom to pending blocks
sendBlocksOut :: SimulationData -> Network -> Node -> Network
sendBlocksOut sd network node = resNetwork
            where
              -- maybe filter by timestamp?
                blocks = pendingBlocks node
                cons = outgoingConnections network node
                clen = length cons
                cons' = if clen == 0 then []
                                     else [cons !! (fst $ randomR (0, clen - 1) $ nodeGen sd node)]
                resNetwork' = foldl (\nw' otherNode ->
                                      foldl (\nw ch -> updateNode (pushBlocks otherNode (makePairs ch)) nw) nw' (map (\bs -> nodeChain (snd bs) node) blocks)
                                      ) network cons'
                resNetwork = if clen == 0 then resNetwork'
                             else updateNode node {pendingBlocks = []} resNetwork'


propagateLastBlocks :: SimulationData -> Network -> Network
propagateLastBlocks sd network = foldl (sendBlocksOut sd) network (nodes network)
  --  where
  --      senders = filter (\n ->
  --              let lastBl = lastNodeBlock n in
  --                  (timestamp sd - blockTimestamp lastBl) < 15 && generator lastBl /= account n
  --          ) (nodes network)

--todo: regulate when to stop transactions to see the convergence (2/3 of deadline for now)
--todo: regulate the range of transactions amount
generateTransactionsForNode :: SimulationData -> Node -> Network -> Node
generateTransactionsForNode sd node network =
      if (selfBalance node >= minFee) then
        let gen = nodeGen sd node in
--        let ns = nodes network in
--        let amt = fst $ randomR (100000 , 1000000) gen in -- fst $ randomR (1 , (selfBalance node) `div` 2) gen in
        let codeIdx = fst $ randomR (0 , length (codeLibrary sd) - 1) gen in
--        let rcp = account $ ns !! (fst $ randomR (0, length ns - 1) gen) in
--        if (rcp /= account node) then
        let tstamp = timestamp sd in
        let tx = Transaction { sender = account node
                               , recipient = godAccount
                               , amount = 0
                               , fee = minFee
                               , txTimestamp = tstamp
                               , payload = codeLibrary sd !! codeIdx
                               } in
          node {pendingTxs = tx:(pendingTxs node)}
--        else node
    else node

-- obsolete?
--generateTransactionsForNodes :: SimulationData -> [Node] -> Network -> [Node]
--generateTransactionsForNodes sd nonEmpty network = map (\n ->
--        let gen = nodeGen sd n in
--        let r::Int = fst $ randomR (0, 10) gen in
--                case r of
--                1 -> generateTransactionsForNode sd n network
--                _ -> n) nonEmpty

-- todo: regulate when to let account to send a transaction
-- todo: move "10" to constants
generateTransactions :: SimulationData -> Network -> Network
generateTransactions sd network = network {nodes = ns} where
                         ns = map (\n -> if (account n == godAccount ) then n else
                                    let gen = nodeGen sd n in
                                    let r::Int = fst $ randomR (0, 50) gen in
                                    case r of
                                     1 -> generateTransactionsForNode sd n network
                                     _ -> n) (nodes network)


networkForge :: SimulationData -> Network -> Network
networkForge sd nw =
    let forgers = map (forgeBlocks (timestamp sd)) (nodes nw) in
    let nwforged = foldl (\nw n -> updateNode n nw) nw forgers in
    nwforged
    -- no need to filter forgers as sending blocks is performed through foldl ... blocks, where the latter can be []


--dirty hack :(
--addInvestorNode :: SimulationData -> Network -> Network
--addInvestorNode sd network = case timestamp sd of
--            1000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 1]}
--           2000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 2]}
--            4000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 3]}
--            5000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 4]}
--            6000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 5]}
--            7000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 6]}
--            7500 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 7]}
--            8000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 8]}
--            9000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 9]}
--            10000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 10]}
--            12000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 11]}
--            14000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 12]}
--            _ -> network


-- propagateLastBlocks can be removed without the loss in convergence ATM ???
systemTransform :: SimulationData -> Network -> Network
systemTransform sd network = networkForge sd $
                             propagateTransactions sd $
                             generateTransactions sd $
                             -- propagateLastBlocks sd $
                             downloadBlocksNetwork sd $
                             -- dropConnections sd $
                             generateConnections sd $
                             addNode sd network
                             -- addInvestorNode sd network


goThrouhTimeline :: (Timestamp -> Network -> IO ()) -> (SimulationData, Network) -> IO (SimulationData, Network)
goThrouhTimeline notifyState (sd, nw) | (notExpired sd) && ((maximum $ map (\n -> accBalance n godAccount) (nodes nw)) > 0) = do
  let nextSimulationData = incTimestamp sd
  let nextNetworkState = systemTransform nextSimulationData nw
  notifyState (timestamp nextSimulationData) nextNetworkState
  goThrouhTimeline notifyState (nextSimulationData, nextNetworkState)
goThrouhTimeline _ (sd, nw) = return (sd, nw)


runSimulation :: (Timestamp -> Network -> IO ()) -> (SimulationData, Network) -> IO Network
runSimulation notifyState (ini, net) = do
  (_, net') <- goThrouhTimeline notifyState (ini, net)
  return net'
