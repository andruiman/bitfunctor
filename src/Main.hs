{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Simulation
import Blockchain
import Transaction
import Theory
import qualified Data.Map as Map
import System.Directory
import Data.ConfigFile as ConfigFile
import Data.Either.Utils
import Block
import Data.Aeson (decode)
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy as BL (readFile)
import Control.Monad (filterM)
import System.Process
import System.Exit
import GHC.IO.Handle
import System.IO
import Control.Concurrent


outChain :: BlockChain -> String
outChain chain = chainInfo ++ diffInfo
    where
        diffInfo = concat ["Cumulative difficulty: ", show $ cumulativeDifficulty chain]
        chainInfo = foldl (\s b -> concat [s, show $ blockTimestamp b, " -> ", show $ generator b, "--",
                                            show $ blockId b ,"--", show $ baseTarget b, "--", show $ length $ transactions b] ++ "\n") "" chain

outChainNode :: Node -> String
outChainNode node = outChain $ bestChain node


outTxs :: Node -> String
outTxs node = foldl (\s t -> concat [s, show t, "\n"]) "" txs
    where
        chain = bestChain node
        txs = concat $ map transactions $ chain

outConnection :: Network -> String
outConnection network = concat ps
    where
        cons = connections network
        ks = Map.keys cons
        ps = map (\k -> concat [show $ nodeId $ k, " -> ", show $ Map.findWithDefault [] k cons, "\n "]) ks

commonChainLength :: Node -> Node -> Int
commonChainLength n1 n2 = length $ commonChain (bestChain n1) (bestChain n2)

commonChainsNode :: Node -> [Node] ->  String
commonChainsNode n ns = show (let others = filter (\_ -> True) ns in map (commonChainLength n) others)

commonChains :: [Node] -> String
commonChains ns = show $ map (\n -> concat[show $ nodeId n," : ",show $ selfBalance n ,"<->", commonChainsNode n ns]) ns

nodeBalances :: Node -> [Node] -> String
nodeBalances node ns = let bals =  map (accBalance node) $ map account ns in
  concat [show (nodeId node), "->" , show $ (sum bals): bals]

allBalances :: [Node] -> String
allBalances nodes = concat $ map (\n -> nodeBalances n nodes ++ "\n") nodes

main = do

    putStrLn "Reading configuration..."

    val <- ConfigFile.readfile ConfigFile.emptyCP "params.conf"
    let cp = forceEither val

    let outDir   = forceEither $ ConfigFile.get cp "DEFAULT" "outdir"
    let stateDir = forceEither $ ConfigFile.get cp "DEFAULT" "statedir"

    dataFiles <- getDirectoryContents "data"
    let dataFilesFull = map (\f -> "data/" ++ f) dataFiles
    codeFiles <- filterM (doesFileExist) dataFilesFull
    files <- mapM BL.readFile codeFiles
    let cl = catMaybes $ map decode files
    let initSimData = SimulationData {
        timestamp = 0,
        simulationId = forceEither $ ConfigFile.get cp "DEFAULT" "simulation-id",
        maxConnectionsPerNode = forceEither $ ConfigFile.get cp "DEFAULT" "max-connections-per-node",
        addNodeAvgGap = forceEither $ ConfigFile.get cp "DEFAULT" "add-node-avg-gap",
        deadline = forceEither $ ConfigFile.get cp "DEFAULT" "duration",
        codeLibrary = cl
      }

    putStrLn "Reading code library..."
    mapM_ print codeFiles
    mapM_ print files
    mapM_ print cl

    createDirectoryIfMissing True outDir
    createDirectoryIfMissing True stateDir

    putStrLn "Starting cryptocurrency simulation..."

    network <- runSimulation (outputResults stateDir) (initSimData, genesisState)

    putStrLn "\nFinal simulation results:"
    outputResults outDir 0 network

    putStrLn "\nCompiling theory:"
    let view = localView $ last (nodes network)
    let th = Map.findWithDefault Map.empty (bestBlock view) (blockTheory view)
    let fname = "sort"    
    let func_name = "Function#" ++ fname
    let funcfile_name = "/" ++ fname ++ "_out.v"
    let fullfuncfile_name = concat [outDir, funcfile_name]
    let thcode = showDependentAtomCode func_name th
    writeFile fullfuncfile_name thcode
    createProcess (proc "coqc" ["-verbose", fullfuncfile_name])

    putStrLn "\nExtracting code:"
    let s = "Extraction Language Haskell.\nExtraction \"" ++ fname ++ "\" " ++ fname ++ "."
    let exname = concat [outDir, "/extract.v"]
    let outname = concat [outDir, "/", fname, "_out"]
    writeFile exname s
    createProcess (proc "coqc" ["-verbose", "-require", outname, exname])
    
    putStrLn "\nCryptocurrency simulation has been finished"


outputResults :: String -> Timestamp -> Network -> IO ()
outputResults outdir ts network = do
  putStrLn ("====================================================================" ++ (show ts))

  let ns = nodes network

  writeFile (concat [outdir, "/network"]) (show network)
  writeFile (concat [outdir, "/cons"]) (outConnection network)
  mapM_ (\i -> writeFile (concat [outdir, "/node", show i]) (show (ns !! i))) [0..(length ns - 1)]
  mapM_ (\i -> writeFile (concat [outdir, "/chain", show i]) (outChainNode $ ns !! i)) [0..(length ns - 1)]
  mapM_ (\i -> writeFile (concat [outdir, "/txs", show i]) (outTxs $ ns !! i)) [0..(length ns - 1)]
  mapM_ (\i -> writeFile (concat [outdir, "/common", show i]) (outChain $ commonChain (bestChain $ ns !! i) (bestChain $ ns !! (i+1)))) [0..(length ns - 2)]


  putStrLn "Balances (from the nodes' point of view):"
  putStrLn $ show $ map selfBalance ns

  putStrLn "Network connections:"
  putStrLn $ show $ (Map.mapKeys nodeId $ connections network)

  putStrLn "PendingTxs:"
  putStrLn $ show $ (map (length.pendingTxs) $ nodes network)

  putStrLn "ProcTxs:"
  putStrLn $ show $ (map (length.procTxs) $ nodes network)

  putStrLn "Open blocks:"
  putStrLn $ show $ (map (length.openBlocks) $ nodes network)

  putStrLn "Blockchain 0:"
  putStrLn $ (outChainNode $ ns !! 0)
 

  putStrLn "\n"
  putStrLn "Node Id : Self balance <-> Common chain lengths with other nodes: "
  let cc = commonChains ns
  writeFile (concat [outdir, "/commons"]) cc
  putStrLn $ cc

  let bb = allBalances ns
  writeFile (concat [outdir, "/balances"]) bb
  putStrLn $ bb
  putStrLn "===================================================================="
