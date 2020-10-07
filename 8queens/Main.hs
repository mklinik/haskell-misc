-- The 8 queens problem
-- A brute-force implementation

type Queen = Int
type Field = [Queen]

-- Strategy:
--  generate all combinations
--  filter the non-consistent ones

combinations = [ [x0, x1, x2, x3, x4, x5, x6, x7]
 | x0 <- [0..7]
 , x1 <- [0..7]
 , x2 <- [0..7]
 , x3 <- [0..7]
 , x4 <- [0..7]
 , x5 <- [0..7]
 , x6 <- [0..7]
 , x7 <- [0..7]
 ]

consistent :: [Int] -> Bool
consistent [] = True
consistent (q:qs) = consistent_ q (7 - length qs) qs && consistent qs

consistent_ :: Int -> Int -> [Int] -> Bool
consistent_ qy qx [] = True
consistent_ qy qx (q:qs) = differentRow && differentDiagonal && consistent_ qy qx qs
 where
  differentRow = qy /= q
  differentDiagonal = abs (q - qy) /= abs ((7 - length qs) - qx)

main = do
  let solution =  head $ filter consistent combinations
  putStrLn $ printBoard $ solution
  print solution

printBoard :: Field -> String
printBoard [] = ""
printBoard (q:qs) = concat (take q $ repeat "| ")
  ++ "|X"
  ++ concat (take (7-q) $ repeat "| ")
  ++ "|\n"
  ++ printBoard qs
