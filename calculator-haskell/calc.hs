import Data.Char

pri = ["(","1","+","2","-","2","*","3","/","3"]
opr = ["+","-","*","/"]
brckt = ["(",")"]

getPri [] _ = "-1"
getPri x y
	| head x == y = x !! 1
	| otherwise = getPri (drop 2 x) y

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
    
infixToRPN [] [] = []
infixToRPN [] [x] = x
infixToRPN [] s@(x:xs) = last s ++ " " ++ infixToRPN [] (init s)
infixToRPN x y 
  | isDigit (head x) || head x == '.' =
      if length x > 1 then
          if isDigit (x !! 1) || x !! 1 == '.' then
              [head x] ++ infixToRPN (tail x) y
          else
              [head x] ++ " " ++ infixToRPN (tail x) y
      else if head x == '.' then
          error "Expression contains invalid number(s)"
      else
          [head x] ++ " " ++ infixToRPN (tail x) y
  | [(head x)] `elem` opr =
  	  if cmpPri y (getPri pri [head x]) == False then
  	      infixToRPN (tail x) (y ++ [[head x]])
      else if getPri pri (last y) >= getPri pri [head x] then
          last y ++ " " ++ infixToRPN x (init y)
      else
          infixToRPN (tail x) (y ++ [[(head x)]])
  | [head x] `elem` brckt =
      if length y == 0 && [(head x)] == brckt !! 1 then
          error "Not enough opening brackets"
  	  else if [head x] == brckt !! 0 then
  	      infixToRPN (tail x) (y ++ [[head x]])
  	  else if last y /= brckt !! 0 then
  	      last y ++ " " ++ infixToRPN x (init y)
	    else
	        infixToRPN (tail x) (init y)
  | otherwise = 
      if (head x) == ' ' then infixToRPN (tail x) y
      else error "Expression contains unresolved characters"

calc [] [x] [] = x
calc [] [] (x:xs) = error "Not enough operands"
calc [] (x:xs) [] = error "Not enough operators"
calc [] [x] (y:ys) = error "Not enough operators"
calc x y z
  | isDigit (head x) || head x == '.' = calc (tail x) y (z ++ [(head x)])
  | [(head x)] `elem` opr =
      if length y >= 2 then
          calc (tail x) ((take (length y - 2) y) ++ [calcNums 
              (y !! (length y - 2)) [(head x)] (y !! (length y - 1))]) ([])
      else
          error "Too many operators"
  | otherwise = 
      if [(head x)] == brckt !! 0 then error "Not enough closing brackets"
      else if length z > 0 then
          calc (tail x) (y ++ [read z :: Double]) ([])
      else
          calc (tail x) y ([])

calculate [] = error "Empty expression"
calculate x = calc (infixToRPN x []) [] []
