module Theory where

import qualified Data.Map as Map
import Data.List
import qualified Transaction as Tx
import Data.Maybe (catMaybes)

type TheoryKind = Tx.PayloadType

data TheoryAtom = TheoryAtom {
 name :: String
, code :: String
, kind :: TheoryKind
, uses :: [TheoryAtom]} deriving (Eq, Show)

type Theory = Map.Map String TheoryAtom

addAtom :: TheoryAtom -> Theory -> Theory
addAtom a t = Map.insert (name a) a t

fetchDependentAtom :: String -> Theory -> [TheoryAtom]
fetchDependentAtom s t = let atoms = let ma = Map.lookup s t in
                                     case ma of
                                       Just a -> (concat $ map (\a' -> fetchDependentAtom (name a') t) (uses a)) ++ [a]
                                       Nothing -> []
                         in nub atoms

showDependentAtomCode :: String -> Theory ->  String
showDependentAtomCode s t = concat $ map code (fetchDependentAtom s t)


--
--  uses     :: [(PayloadType, String)],
--  provides :: (PayloadType, String),
--  code     :: String

toName :: (TheoryKind, String) -> String
toName (k,s) = (show $ k) ++ "#" ++ s

toAtom :: Tx.VerifiableTransactionPayload -> Theory -> TheoryAtom
toAtom vtxp t = TheoryAtom {name = toName $ Tx.provides vtxp,
                            code = Tx.code vtxp,
                            kind = fst $ Tx.provides vtxp,
                            uses = catMaybes $ map (\i -> Map.lookup (toName i) t) $ Tx.uses vtxp}

atomComplexity :: TheoryAtom -> Int
atomComplexity a = case (kind a) of
                     Tx.Function -> 10
                     Tx.Type -> 1
                     Tx.Theorem -> 30

theoryComplexity :: Theory -> Int
theoryComplexity t =  Map.foldr (\a c -> c + (atomComplexity a)) 0 t
