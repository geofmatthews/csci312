
scoreToLetter :: Int -> Char

scoreToLetter n
  | n > 90 = 'A'
  | n > 80 = 'B'
  | n > 70 = 'C'
  | otherwise = 'F'
  


