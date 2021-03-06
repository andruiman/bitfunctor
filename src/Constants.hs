module Constants where

minFee :: Int
minFee = 0

goalBlockTime :: Integer
goalBlockTime = 60

maxHit :: Integer
maxHit = 2^64

systemBalance :: Int
systemBalance = 14000000

two64 :: Double
two64 = 2**64

maxBlocksFromPeer :: Int
maxBlocksFromPeer = 10*1440

complexityPrice :: Int
complexityPrice = 1000000

-- tfdepth > 1 finite multibranch 
--         = 1 singlebranch                                                                              
--         = 0 not forging                                                                               
--         < 0 full multibranch (please do not use - exponential growth) 
tfDepth :: Int
tfDepth = 1
