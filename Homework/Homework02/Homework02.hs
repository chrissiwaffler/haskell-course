
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: String -> String -> String -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
-- > readability for other developers, also fix ambiguities for the compiler

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- > similar to the previous question; better readability for yourself so you avoid mistakes and maybe clarify it for the compiler

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- > can be converted with a specific function, not a real cast

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- > define them by:
l1 = [[3, 2], [2, 3]]
-- or
l2 = [3, 2] : ([2, 3] : [[]])

-- access with
e1 = head (head l1)
-- or
e2 = (l1 !! 1) !! 1