module Theory where

import qualified Data.Map as Map
import Data.List
import qualified Transaction as Tx

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
                                       Just a -> a:(concat $ map (\a' -> fetchDependentAtom (name a') t) (uses a))
                                       Nothing -> []
                         in nub $ reverse atoms

showDependentAtomCode :: String -> Theory ->  String
showDependentAtomCode s t = concat $ map code (fetchDependentAtom s t)

toAtom :: Tx.VerifiableTransactionPayload -> TheoryAtom

                         
