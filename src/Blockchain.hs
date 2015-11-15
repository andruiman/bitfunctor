module Blockchain where

import Constants
import Account
import Block
import Transaction
import Theory
import Utils


import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as BI
import qualified Data.Map as Map


-- Account-based proof-of-stake cryptocurrency model


validate :: Transaction -> Bool
validate _ = True


-- todo: add more checks?
isGenesis :: Block -> Bool
isGenesis b = blockTimestamp b == 0


initialBaseTarget :: Integer
initialBaseTarget = div maxHit (2*goalBlockTime*(fromIntegral systemBalance))

maxBaseTarget :: Integer
maxBaseTarget = initialBaseTarget * (fromIntegral systemBalance)

-- rewritten as we now maintain the whole history, better do it by Map?
containsTx :: Block -> LocalView -> Transaction -> Bool
containsTx block view tx = elem tx $ Map.findWithDefault [] block $ blockTransactions view

calcHash :: [Integer] -> B.ByteString
calcHash l = SHA.bytestringDigest $ SHA.sha256 $ B.pack $ map fromIntegral l

calcGenerationSignature :: Block -> Account -> B.ByteString
calcGenerationSignature prevBlock acct = SHA.bytestringDigest $ SHA.sha256 $ B.append (generationSignature prevBlock) $ publicKey acct

--correct this!
difficultyFunction :: Integer -> Double
difficultyFunction x = 20e8/(fromIntegral x)

formBlock :: Block -> Account -> Timestamp -> [Transaction] -> Block
formBlock prevBlock gen timestamp txs =
    Block {transactions = newTxs, blockTimestamp = timestamp,
           baseTarget = bt, totalDifficulty = td, generator = gen,
           generationSignature = gs}
    where newTxs = filter validate txs
          prevTarget = baseTarget prevBlock
          maxTarget = min (2*prevTarget)  maxBaseTarget
          minTarget = max (prevTarget `div` 2)  1
          candidate = prevTarget*(toInteger (timestamp - (blockTimestamp prevBlock))) `div` goalBlockTime
          bt = min (max minTarget candidate) maxTarget
          gs = calcGenerationSignature prevBlock gen -- is gs 64 bytes?
          -- move to the caller?
          td = (totalDifficulty prevBlock) + (difficultyFunction bt)

type BlockChain = [Block]

-- from the next to parent
type BlockTree = Map.Map Block Block

cumulativeDifficulty :: BlockChain -> Double
cumulativeDifficulty chain = foldl (\cd b -> cd + (difficultyFunction $ baseTarget b)) 0 chain

cumulativeNodeDifficulty :: Node -> Double
cumulativeNodeDifficulty node = totalDifficulty $ bestBlock $ localView node

-- type BlockTree = [BlockChain]

type Ledger = Map.Map Account Int

data LocalView =
    LocalView {
        blockTree :: BlockTree,
        diffThreshold :: Double,
        blockBalances :: Map.Map Block Ledger,
        blockTheory :: Map.Map Block Theory,
        bestBlock :: Block,
        -- all the transactions from the genesis, better do it as Block->Transaction mapping?
        blockTransactions :: Map.Map Block [Transaction]
    } deriving (Show)


accountBalance :: LocalView -> Block -> Account -> Int
accountBalance view b acc = Map.findWithDefault 0 acc $ Map.findWithDefault Map.empty b $ blockBalances view

effectiveBalance :: LocalView -> Block -> Account -> Int
effectiveBalance view b acc = accountBalance view b acc -- todo: simplification, no 1440 blocks waiting for now

addMoney ::  Int -> Account ->  Ledger -> Ledger
addMoney diff acc blns = let oldBalance = Map.findWithDefault 0 acc blns in
                         Map.insert acc (oldBalance + diff) blns

-- added -(fee tx) to sender balance
-- the order of arguments is changed to simplify later foldl's
applyTx :: (Ledger, Theory) -> Transaction  -> (Ledger, Theory)
applyTx (blns, th) tx = (bals2, th1)
    where
        bals1 = addMoney amt (recipient tx) $ addMoney (-amt-(fee tx)) (sender tx) blns
        ma = toAtom (payload tx) th
        case ma of
          Just a -> 
                 th1 = addAtom (toAtom (payload tx) th) th
                 deltaCmpl = (theoryComplexity th1) - (theoryComplexity th)
                 bonus = complexityPrice*deltaCmpl
                 bals2 = addMoney (-bonus) godAccount $ addMoney bonus (sender tx) bals1
                 amt = amount tx

processBlock :: Block -> (Ledger, Theory) -> (Ledger, Theory)
processBlock block (priorBalances, priorTheory) =
    (feesApplied, txAppliedT)
    where
        txs = transactions block
        (txAppliedL, txAppliedT) = foldl applyTx (priorBalances, priorTheory) txs        
        fees = sum (map fee txs)
        feesApplied = addMoney fees (generator block) txAppliedL
        
--processTheory :: Block -> Theory -> Theory
--processTheory b t = foldl (\t tx -> addAtom (toAtom (payload tx) t) t) t $ transactions b

deltaThreshold = 7

pushBlock :: Node -> Block -> Block -> Node
pushBlock node pb b =  let view = localView node in
                       if (Map.notMember b $ blockTree view) then
                        if (Map.member pb $ blockTree view) || (isGenesis pb) then
                           let prBal = Map.findWithDefault Map.empty pb $ blockBalances view in
                           let prTh = Map.findWithDefault Map.empty pb $ blockTheory view in                           
                           let prTxs = Map.findWithDefault []       pb $ blockTransactions view in
                           let opb = addSortedBlock b (openBlocks node) in
                           let bb' = head opb in
                           let (newBal, newTh) = processBlock b (prBal, prTh) in
                           let pTxs = pendingTxs node in
                           let updView = view {
                              blockTree         = Map.insert b pb $ blockTree view,
                              blockBalances     = Map.insert b newBal $ blockBalances view,
                              blockTransactions = Map.insert b (prTxs ++ (transactions b)) $ blockTransactions view,
                              blockTheory       = Map.insert b newTh $ blockTheory view,
                              bestBlock =  let oldbb = bestBlock view in
                                            if (totalDifficulty bb' >= totalDifficulty oldbb) then bb' else oldbb,
                              diffThreshold = let olddt = diffThreshold view in
                                              if (totalDifficulty bb' - olddt >= deltaThreshold) then olddt + deltaThreshold
                                                                                                 else olddt} in
                           node {localView = updView, pendingBlocks = (pb,b):(pendingBlocks node),
                                 openBlocks = opb, pendingTxs = filter (\tx -> notElem tx (transactions b)) pTxs }
                        -- filter (\tx -> notElem tx (transactions b)) pTxs
                        -- need to add more logic when prevBlock not found - try to download it or whatever
                        else node
                       else node

pushBlocks :: Node -> [(Block, Block)] -> Node
pushBlocks = foldl (\n (pb,b) -> pushBlock n pb b)

procTxs :: Node -> [Transaction]
procTxs node = let mlt = Map.lookup (bestBlock $ localView node) (blockTransactions $ localView node) in
               case mlt of
                Just lt -> lt
                Nothing -> []

data Node =
    Node {
        localView :: LocalView,
        -- renamed to exclude inappropriate usage
        pendingTxs :: [Transaction],
        -- processedTxs :: [Transaction],
        openBlocks :: [Block],
        pendingBlocks :: [(Block,Block)],
        account :: Account -- simplification - one account per node
        --isForging :: Bool - simplification - always on for now
    }  deriving (Show)

nodeId :: Node -> Int
nodeId node = accountId $ account node

instance Eq Node where n1 == n2  = nodeId n1 == nodeId n2
instance Ord Node where compare n1 n2 = compare (nodeId n1) (nodeId n2)

--lastNodeBlock :: Node -> Block
--lastNodeBlock nd = last $ nodeChain $ localView nd

--nodeChainLength :: Node -> Int
--nodeChainLength nd = length $ nodeChain $ localView nd

processIncomingBlock :: Node -> Block -> Block -> Node
processIncomingBlock node pb block = updNode
    where
        sigHash = first8bytesAsNumber $ calcGenerationSignature pb (generator block)
        -- todo: many other checks!!
        updNode = case first8bytesAsNumber (generationSignature block) == sigHash of
            True -> pushBlock node pb block
            False -> node


accBalance :: Node -> Account -> Int
accBalance node acc = let v = localView node in
                      accountBalance v (bestBlock v) acc

selfBalance :: Node -> Int
selfBalance node = accBalance node (account node)

calculateHit :: Block -> Account -> Integer
calculateHit prevBlock acc = fromIntegral $ first8bytesAsNumber $ calcGenerationSignature prevBlock acc


-- hitTime :: Account -> Block -> LocalView -> Timestamp
-- hitTime acct prevBl view = (blockTimestamp prevBl) + (calculateHit prevBl acct)*(effectiveBalance view acct) `div` (baseTarget prevBl)


verifyHit :: Integer -> Block -> Timestamp -> Int -> Bool
verifyHit hit prevBlock timestamp effBalance = (eta > 0) && hit < target -- && hit >= prevTarget) - after block 215000
    where eta = timestamp - blockTimestamp prevBlock
          effbt = (toInteger effBalance)*(baseTarget prevBlock)
          target = effbt*(toInteger eta)
          -- prevTarget = effbt * (eta-1)

-- what is better (>) or (>=)?
addSortedBlock ::  Block -> [Block] -> [Block]
addSortedBlock b [] = [b]
addSortedBlock b lb@(b':bs) =  if ((totalDifficulty b) >= (totalDifficulty b')) then b:lb
                                                           else b':(addSortedBlock b bs)


forgeBlock :: Block -> Node -> Timestamp -> Node
forgeBlock pb node ts =
   let view  = localView node in
   let prTxs = Map.findWithDefault [] pb $ blockTransactions view in
   let th = Map.findWithDefault Map.empty pb $ blockTheory view in
-- TODO: add validation txs with th
   let txs  = filter (\tx -> notElem tx prTxs) $ pendingTxs node in
   let acct = account node in
   let effb = effectiveBalance view pb acct in
   let hit  = calculateHit pb acct in
   let checkHit = verifyHit hit pb ts effb in
   let openb = openBlocks node in
      if checkHit then let newb = formBlock pb acct ts txs in
                        pushBlock node pb newb
                  else node {openBlocks = addSortedBlock pb openb}


splitBlocks :: Int -> [Block]  -> ([Block], [Block])
splitBlocks k lb  | k < 0 = (lb, [])
--splitBlocks 0 lb  = (defl, [])
splitBlocks k lb  | k >= 0 = splitAt k lb

forgeBlocks ::  Timestamp -> Node -> Node
forgeBlocks ts node = let acc = account node in
                      let view = localView node in
                      let opb = openBlocks node in
                      let (blocks, rb) = splitBlocks (tfdepth acc) opb in
                      let bs = filter (\b -> totalDifficulty b >= diffThreshold view) blocks in
                      let node' = node {openBlocks = []} in
                      foldl (\n pb -> forgeBlock pb n ts) node' blocks

treeChain :: Block -> BlockTree -> BlockChain
treeChain b t = if Map.member b t then
                  let pb = Map.findWithDefault b b t in
                           (treeChain pb t) ++ [b]
                else [b]

nodeChain :: Block -> Node -> BlockChain
nodeChain b node = let view = localView node in
                   let tree = blockTree view in
                   treeChain b tree


bestChain :: Node -> BlockChain
bestChain node = let view = localView node in
                 treeChain (bestBlock view) (blockTree view)


-- removed accumulator common to reduce (++) operations
commonChain :: BlockChain -> BlockChain -> BlockChain
commonChain chain1 chain2 = case (chain1, chain2) of
        (bl1:ct1, bl2:ct2) -> if bl1 == bl2 then bl1:(commonChain ct1 ct2) else []
        _ -> []

-- Nodes are mutable !!! so map works wrong, changed [Node] to [Int]
data Network =
    Network {
        nodes :: [Node],
        connections :: Map.Map Node [Int] -- todo add latency(avg latency time), trust?
    }  deriving (Show)


outgoingConnections :: Network -> Node -> [Node]
outgoingConnections network node = let ids = outgoingConnectionsIds network node in
                                   filter (\n -> elem (nodeId n) ids) (nodes network)

outgoingConnectionsIds :: Network -> Node -> [Int]
outgoingConnectionsIds network node = Map.findWithDefault [] node (connections network)


updateNode :: Node -> Network -> Network
updateNode nd network = network {nodes = ns}
            where
            ndId = nodeId nd
            ns = map (\n -> if (nodeId n == ndId) then nd else n) (nodes network)

--updateNode nd network = network {nodes = ns}
--    where
      -- (==) for nodes as Ids
--        ns = map (\n -> if (n == nd) then nd else n) (nodes network)


--blockTree :: Network -> BlockTree
--blockTree sys = error "not impl"

-- todo: define canonical blockchain and implement its extraction from blocktree of a system
canonicalBlockchain :: Network -> Maybe BlockChain
canonicalBlockchain sys = Nothing
