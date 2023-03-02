-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

checkDeviceInLimit :: Double -> Double -> Double -> String
checkDeviceInLimit consumption usage limit
    | monthlyUsage == limit = "equal to the limit"
    | monthlyUsage > limit = "bigger than the limit"
    | otherwise = "smaller than the limit"
    where
        monthlyUsage = consumption * usage * 30


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
getDifferenceToLimit consumption usage limit =
    let monthlyUsage = consumption * usage * 30
    in let diff = abs (limit - monthlyUsage)
    in let text = checkDeviceInLimit consumption usage limit
    in if diff == 0 
        then text
        else show diff ++ " kW " ++ text

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
-- > see the function above


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
makeQuotient a b
    | absA < absB = show (a / b)
    | absB < absA = show (b / a)
    | otherwise = "division not possible because both numbers are zero"
    where 
        absA = abs a
        absB = abs b

-- with if-then-else
makeQuotient' a b =
    if a == 0 && b == 0
        then "division not possible because both numbers are zero"
        else if absA < absB
            then show (a / b)
            else show (b / a)
    where 
        absA = abs a
        absB = abs b


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 

-- > where block inside a let expression
question5 :: Double -> Double -> Double
question5 a b =
    let productSquare = sqrt product
        quotientSquare = sqrt quotient
    in productSquare + quotientSquare
    where 
        product = a * b
        quotient = a / b

-- > let expression inside a where block
question5' a b = 
    productSquare + quotientSquare 
    where 
        productSquare = 
            let product = a * b 
            in sqrt product
        quotientSquare = 
            let quotient = a / b 
            in sqrt quotient

