import Data.Char

pri = ["(","1","+","2","-","2","*","3","/","3"]
opr = ["+","-","*","/"]
brckt = ["(",")"]

getPri :: [[Char]] -> [Char] -> [Char]
getPri [] _ = "-1"
getPri x y
	| head x == y = x !! 1
	| otherwise = getPri (drop 2 x) y

cmpPri :: [[Char]] -> [Char] -> Bool
cmpPri [] _ = False
cmpPri y pr
  | getPri pri (last y) >= pr = True
	| otherwise = cmpPri (init y) pr

calcNums :: Double -> [Char] -> Double -> Double
calcNums x op y
  | op == "+" = x + y
  | op == "-" = x - y
  | op == "*" = x * y
  | op == "/" = x / y
  | otherwise = 0

processNum :: [Char] -> [[Char]] -> [Char]
processNum str stk 
  | length str > 1 =
      if isDigit (str !! 1) || str !! 1 == '.' then
          [head str] ++ infixToRPN (tail str) stk
      else
          [head str] ++ " " ++ infixToRPN (tail str) stk
  | head str == '.' = error "Expression contains invalid number(s)"
  | otherwise = [head str] ++ " " ++ infixToRPN (tail str) stk

processOpr :: [Char] -> [[Char]] -> [Char]
processOpr str stk 
  | not (cmpPri stk (getPri pri [head str])) =
      infixToRPN (tail str) (stk ++ [[head str]])
  | getPri pri (last stk) >= getPri pri [head str] =
      last stk ++ " " ++ infixToRPN str (init stk)
  | otherwise = infixToRPN (tail str) (stk ++ [[(head str)]])

processBrckt :: [Char] -> [[Char]] -> [Char]
processBrckt str stk 
  | length stk == 0 && [(head str)] == brckt !! 1 =
      error "Not enough opening brackets"
  | [head str] == brckt !! 0 =
      infixToRPN (tail str) (stk ++ [[head str]])
  | last stk /= brckt !! 0 =
      last stk ++ " " ++ infixToRPN str (init stk)
  | otherwise = infixToRPN (tail str) (init stk)

infixToRPN :: [Char] -> [[Char]] -> [Char]
infixToRPN [] [] = []
infixToRPN [] [x] = x
infixToRPN [] s@(x:xs) = last s ++ " " ++ infixToRPN [] (init s)
infixToRPN str stk
  | isDigit (head str) || head str == '.' = processNum str stk
  | [(head str)] `elem` opr = processOpr str stk
  | [head str] `elem` brckt = processBrckt str stk
  | otherwise = if (head str) == ' ' then infixToRPN (tail str) stk
      else error "Expression contains unresolved characters"

calc :: [Char] -> [Double] -> [Char] -> Double
calc [] [x] [] = x
calc [] [] (x:xs) = error "Not enough operands"
calc [] (x:xs) [] = error "Not enough operators"
calc [] [x] (y:ys) = error "Not enough operators"
calc str res stk
  | isDigit (head str) || head str == '.' = 
      calc (tail str) res (stk ++ [(head str)])
  | [(head str)] `elem` opr =
      if length res >= 2 then
          calc (tail str) ((take (length res - 2) res) ++ [calcNums 
          (res !! (length res - 2)) [(head str)] 
          (res !! (length res - 1))]) ([])
      else error "Too many operators"
  | otherwise = 
      if [(head str)] == brckt !! 0 then 
          error "Not enough closing brackets"
      else if length stk > 0 then
          calc (tail str) (res ++ [read stk :: Double]) ([])
      else
          calc (tail str) res ([])

calculate :: [Char] -> Double
calculate [] = error "Empty expression"
calculate str = calc (infixToRPN str []) [] []

