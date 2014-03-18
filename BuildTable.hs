module BuildTable(first, findNext) where
import GenLR
import Data.List

first :: Part -> [Rule] -> [TerminalPart]
first (Left x) _ = [x]
first t@(Right x) rs = concat $ map (\r -> first (head (_right r)) rs) $ filter (\r -> (_left r) == x && (head (_right r)) /= t) rs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

findNext :: (Eq a) => a -> [a] -> Maybe a
findNext _ [] = Nothing
findNext a (x:xs) 
  | a == x = safeHead xs
  | otherwise = findNext a xs

justOnly :: [Maybe a] -> [a]
justOnly [] = []
justOnly (x:xs) = case x of
                    Just a -> a : justOnly xs
                    Nothing -> justOnly xs



follow :: NonterminalPart -> [Rule] -> [TerminalPart]
follow S _  = [EOL]
follow x rs = union patt1 patt2
  where
    patt1 = concat $ map (\y -> first y rs) $ filter (\y -> y /= (Right x)) $ justOnly $ map (\r -> findNext (Right x) (_right r)) rs
    patt2 = concat $ map (\r -> follow (_left r) rs) $ filter (\r -> (last (_right r)) == (Right x) && (_left r) /= x) rs



