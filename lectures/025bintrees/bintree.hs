--   Bintree from EOPL p. 46 in Haskell
--   Illustrating datatypes
--   Geoffrey Matthews
--   Tested in HUGS
--   Thu Apr  4 10:25:57 PST 2002  

data BinTree = Leaf Int |
               InteriorNode String BinTree BinTree

printTree :: BinTree -> IO ()
printTree (Leaf datum) = putStr (show datum)
printTree (InteriorNode key left right) =
  putStr (key ++ "\n" ++ (stringTree left 1) ++ (stringTree right 1))

spaces :: Int -> String
spaces n
  | n == 0    = ""
  | otherwise = "  " ++ spaces (n-1)
  
stringTree :: BinTree -> Int -> String
stringTree (Leaf datum) depth =
  (spaces depth) ++ (show datum) ++ "\n"
stringTree (InteriorNode key left right) depth =
  (spaces depth) ++ key ++ "\n" ++
  (stringTree left (depth + 1)) ++ (stringTree right (depth + 1))


leafSum :: BinTree -> Int
leafSum (Leaf datum) = datum
leafSum (InteriorNode key left right) = (leafSum left) + (leafSum right)

x = (InteriorNode "Fuzzy"
                  (InteriorNode "Wuzzy"
                                (Leaf 11)
	                        (InteriorNode "Was"
	                                      (Leaf 22)
	                                      (Leaf 33)))
                  (InteriorNode "A"
                                (InteriorNode "Bear"
                                              (Leaf 44)
                                              (Leaf 55))
                                (Leaf 66)))








