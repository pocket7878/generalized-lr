module GotoIsland where

import qualified GenLR as G
import qualified BuildTable as B

data Dot = Dot
type GotoItem = Either Dot G.Part
data GotoState = ParseState {
                _left :: G.NonterminalPart,
                _right :: [GotoItem]
                }
data GotoIsland = GotoIsland Int [ParseState]
                
buildGotoGraph :: [G.Rule] -> [GotoIsland]
